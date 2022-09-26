package net.noresttherein.sugar.reflect

import java.{lang => j}
import java.lang.reflect.Method
import java.lang.reflect.Modifier.{ABSTRACT, FINAL, PROTECTED, PUBLIC}

import scala.annotation.tailrec
import scala.reflect.runtime.universe.{runtimeMirror, typeOf, Symbol, TermName, Type, TypeTag}
import scala.collection.concurrent.{Map => ConcurrentMap, TrieMap => ConcurrentTrieMap}
import scala.collection.mutable

import net.bytebuddy.dynamic.scaffold.subclass.ConstructorStrategy.ForDefaultConstructor
import net.bytebuddy.implementation.MethodDelegation.to
import net.bytebuddy.ByteBuddy
import net.bytebuddy.description.method.MethodDescription
import net.bytebuddy.dynamic.loading.{ByteArrayClassLoader, ClassLoadingStrategy}
import net.bytebuddy.dynamic.scaffold.subclass.ConstructorStrategy.Default.NO_CONSTRUCTORS
import net.bytebuddy.implementation.bind.annotation.{Origin, RuntimeType, This}
import net.bytebuddy.implementation.FixedValue
import net.bytebuddy.implementation.MethodCall.invoke
import net.bytebuddy.matcher.ElementMatchers.{isConstructor, not}




private[reflect] object InvocationReflection {

	private[reflect] case class Trace(owner :Any, method :Method, returns :Type) {
		override def toString :String = s"$owner.$method :$returns"
	}


	/** Applies the given function to a mock of type `S` which records all calls and recursively returns similar mocks.
	  * @return a a list of call frames resulting from calling `property` on its argument (first frame on top).
	  * @throws PropertyReflectionException if any of the following happens:
	  *
	  *                                     - runtime code generation fails;
	  *                                     - mocked classes have inaccessible constructors or are final
	  *                                     - passed function calls a non-zero argument method
	  *                                     - passed function calls two methods on the same object
	  *                                     - passed function doesn't return the value returned by the last recorded call frame.
	  */
	def apply[S :TypeTag, T](property :S => T) :List[Trace] = {
		var root :Any = null
		def errorMsg(msg :String) =
			s"""Failed to trace a property call on ${typeOf[S]} starting with "${tracedPropertyName(root)}" ($property):\n$msg"""

		try {
			root = mock(typeOf[S])
			val res = property(root.asInstanceOf[S])

			@tailrec
			def retrace(trace :Trace, parent :Type, result :mutable.Builder[Trace, List[Trace]]) :List[Trace] = {
				val mock = trace.owner
				val next = try {
					mock.getClass.getField(TraceFieldName).get(trace.owner).asInstanceOf[Trace]
				} catch {
					case _ :Exception => null
				}
				if (next == null)
					if (mock == res)
						(result += trace).result()
					else
						throw new PropertyReflectionException(
							s"Value returned by the function ($res) is not the value returned by the last method call.")
				else
					retrace(next, trace.returns, result += trace)
			}

			val traceField = root.getClass.getField(TraceFieldName)
			if (traceField == null)
				throw new PropertyReflectionException(
					s"Mock created for the argument type does not have the trace field $TraceFieldName: $root. $BugInfo")

			val trace = traceField.get(root).asInstanceOf[Trace]

			if (trace == null)
				if (if (typeOf[S] <:< typeOf[AnyVal]) root == res else root.asInstanceOf[AnyRef] eq res.asInstanceOf[AnyRef])
					Nil
				else
					throw new PropertyReflectionException(
						s"Function doesn't call any (non final) methods on its argument.")
			else
				retrace(trace, typeOf[S], List.newBuilder)

		} catch {
			case e :ClassCastException =>
				throw new PropertyReflectionException(errorMsg(
					s"Couldn't create a mock of the correct class (likely due to an abstract return type of a method).\n${e.getMessage}"),
				e)
			case e :ScalaReflectionException =>
				throw new PropertyReflectionException(errorMsg(
					s"Couldn't determine class for an abstract type. Either a return type of last method call " +
					s"is not fully instantiated in the place of method declaration or you have found a bug.\n${e.getMessage}"),
				e)
			case e :Exception =>
				throw new PropertyReflectionException(errorMsg(e.getMessage), e)
		}
	}




	private[InvocationReflection] class MethodInterceptor { //ByteBuddy requires that it is visible in run time to instrumented classes

		@RuntimeType
		def intercept(@This owner :Any, @Origin method :Method): Any = {
			if (method.getParameterCount > 0)
				throw new PropertyReflectionException(s"Intercepted a call on ${owner.getClass} to a method with parameters: $method.")
			val ownerClass = owner.getClass
			val ownerType = try {
				ownerClass.getMethod(TypeMethodName).invoke(owner).asInstanceOf[Type]
			} catch {
				case _ :NoSuchMethodException =>
					throw new PropertyReflectionException(s"Intercepted a call on ${owner.getClass} without a $TypeMethodName method: $method. $BugInfo")
				case e :SecurityException =>
					throw new PropertyReflectionException("Can't access recorded type information via reflection: " + e.getMessage, e)
			}

			val traceField = try {
				ownerClass.getField(TraceFieldName)
			} catch {
				case _ :NoSuchFieldException =>
					throw new PropertyReflectionException(s"Intercepted a call on ${owner.getClass} without a $TraceFieldName: $method. $BugInfo")
				case e :SecurityException =>
					throw new PropertyReflectionException("Can't access call records via reflection: " + e.getMessage, e)
			}
			val prev = traceField.get(owner).asInstanceOf[Trace]
			if (prev != null)
				throw new PropertyReflectionException(s"Intercepted a second call on object ${owner.getClass}: $method. Previous call was: ${prev.method}.")

			val methods = ownerType.member(TermName(method.getName)).alternatives.filter(validPropertyMethod)
			val returnType = methods match {
				case Seq() =>
					throw new PropertyReflectionException(s"Couldn't find a method symbol for $method in type $ownerType of ${owner.getClass}. $BugInfo")
				case Seq(m) =>
					m.asMethod.returnType.dealias.asSeenFrom(ownerType, m.asMethod.owner.asClass)
				case _ =>
					throw new IllegalArgumentException(s"Multiple method symbols for $method in type $ownerType of ${owner.getClass}: $methods.")
			}
			val next = mock(returnType)
			traceField.set(owner, new Trace(next, method, returnType))
			next
		}

		private[this] def validPropertyMethod(s :Symbol) :Boolean = s.isMethod && s.asMethod.paramLists.flatten.isEmpty

	}






	private def mock(subjectType :Type) :Any = {
		val subjectClass = classFor(subjectType)
		if (!subjectClass.isPrimitive && (subjectClass.getModifiers & FINAL) == 0)
			tracer(subjectType.dealias, subjectClass)
		else
			createInstance(subjectClass)
	}



	private def tracer(subjectType :Type, subjectClass :Class[_]) :Any = {
		var mockClass = TracerCache.get(subjectType).orNull
		if (mockClass == null) subjectClass.synchronized {
			mockClass = TracerCache.get(subjectType).orNull
			if (mockClass == null) {
				mockClass = createTracerClass(subjectType, subjectClass)
				TracerCache.put(subjectType, mockClass)
			}
		}
		mockClass.getConstructor().newInstance()
	}



	private def createTracerClass(tpe :Type, clazz :Class[_]) :Class[_] = {
//		val name = clazz.getName + TracerClassNameSuffix + tpe.typeSymbol.fullName.replace('.', '_')
		val tpeSuffix = tpe.toString.replace('.', '_')
			.replace("[", "$q$").replace("]", "$p$") //
		val name = clazz.getName + TracerClassNameSuffix + tpeSuffix
		val mockClass = ClassSubstitutions(clazz)
		val builder =
			if (mockClass.isInterface)
				ByteBuddy.subclass(mockClass, new ForDefaultConstructor).name(name)
			else {
				val constructors = mockClass.getDeclaredConstructors
					.filter { c => (c.getModifiers & (PUBLIC | PROTECTED)) != 0 }.sortBy(_.getParameterCount)
				if (constructors.isEmpty)
					throw new PropertyReflectionException(
						s"Can't instrument a tracer for class ${mockClass.getName} as it has no accessible constructor."
					)
				(new ByteBuddy().subclass(mockClass, NO_CONSTRUCTORS).name(name) /: constructors) {
					(buddy, constructor) =>
						buddy.defineConstructor(PUBLIC).intercept(
							invoke(constructor).onSuper.`with`(constructor.getParameterTypes.map(createInstance):_*)
						)
				}
			}
		builder //todo: discover cycles in constructors
			.method(not(isConstructor[MethodDescription]())).intercept(to(new MethodInterceptor()))
			.defineMethod(TypeMethodName, classOf[AnyRef], PUBLIC).intercept(FixedValue.value(tpe))
			.defineField(TraceFieldName, classOf[Trace], PUBLIC) //todo: my own loader and INJECTION strategy
			.make().load(classLoader, ClassLoadingStrategy.Default.INJECTION).getLoaded
	}



	/** Creates a ''non-tracing'' instance of the given class to use as an argument for a constructor of some other class.
	  * It will first check if `clazz` is one of the value classes or other common classes and, if so, use a preset value.
	  * Otherwise if the class is not final it will try all of its public constructors.
	  * If all fails, it will resort to instrumenting a subclass using its constructor with the fewest arguments.
	  * This method tries to take into account member and local classes and first recursively create an instance
	  * of the enclosing class for the implicit first parameter.
	  * @param clazz a class we need an instance of. Can't be null.
	  * @return an instance of `clazz` (possibly boxed if `clazz` is a primitive), or null if all attempts failed.
	  */
	private def createInstance(clazz :Class[_]) :AnyRef = {
		val preset = dummyValue(clazz)
		if (preset != null) //first try values for common classes like Option, String, Int, etc.
			preset //todo: mark for the future if we failed to provide a result.
		else if (clazz.isInterface) //todo: abstract methods!!!
		     new ByteBuddy().subclass(clazz).name(clazz.getName + MockClassNameSuffix)
			     .make().load(classLoader).getLoaded
		else if ((clazz.getModifiers & ABSTRACT) != 0)
		     instrumentedInstance(clazz)
		else { //the class is not abstract, so we'll just try all constructors to see if one works
			val constructors = clazz.getConstructors.sortBy(_.getParameterCount).to(LazyList) //list of public constructors
			if (constructors.isEmpty)
				instrumentedInstance(clazz, PROTECTED) //resort to instrumenting a subclass
			else
				constructors.map { cons =>
					util.Try {
						cons.newInstance(cons.getParameterTypes.map(createInstance) :_*)
					}.toOption //if we fail there is little chance that instrumenting will work, so we give up
				}.collectFirst { case Some(instance) => instance.asInstanceOf[AnyRef] }.orNull
		}
	}



	/** Finds a constructor of `clazz` with the lowest number of arguments with at least one access modifier from `modifiers`
	  * and instruments a subclass calling that constructor. The instrumented instance will '''not''' be a traced instance.
	  * @param clazz the class to mock, can't be final.
	  * @param modifiers bitwise or of access modifiers (at least one of `PUBLIC` and `PROTECTED`).
	  * @return an instance of instrumented subclass of `clazz`
	  *         or null if there is no accessible constructor or `clazz` is final.
	  */
	private def instrumentedInstance(clazz :Class[_], modifiers :Int = PUBLIC | PROTECTED) :AnyRef =
		if ((clazz.getModifiers & FINAL) != 0)
			null
		else {
			val mockClass = MockCache.getOrElse(clazz,
				clazz.synchronized {
					MockCache.getOrElse(clazz, {
						clazz.getDeclaredConstructors.filter { c => (c.getModifiers & modifiers) != 0 }
							.sortBy(_.getParameterCount).headOption.map { cons =>
							val subclass = ByteBuddy.subclass(clazz, NO_CONSTRUCTORS)
								.name(clazz.getName + MockClassNameSuffix)
								.defineConstructor(PUBLIC).intercept(
								invoke(cons).onSuper.`with`(cons.getParameterTypes.map(createInstance) :_*)
							).make().load(classLoader, ClassLoadingStrategy.Default.INJECTION).getLoaded
							MockCache.put(clazz, subclass)
							subclass
						}.orNull
					})
				}
			)
			mockClass.getConstructor().newInstance().asInstanceOf[AnyRef]
		}



	/** Last resort dummy instances of primitive and common final classes.  */
	private def dummyValue(subjectClass :Class[_]) :AnyRef =
		(subjectClass match {
			//todo: random
			case JavaString => "42"
			case j.Integer.TYPE | JavaInteger => 42
			case j.Long.TYPE | JavaLong => 42L
			case j.Double.TYPE | JavaDouble => 42.0
			case j.Boolean.TYPE | JavaBoolean => true
			case j.Byte.TYPE | JavaByte => 42.toByte
			case j.Character.TYPE | JavaChar => 42.toChar
			case j.Float.TYPE | JavaFloat => 42F
			case j.Short.TYPE | JavaShort => 42.toShort
			case j.Void.TYPE | ScalaUnit => ()
			//			case ScalaOption => None
			case _ => try {
				subjectClass.getConstructor().newInstance()
			} catch {
				case _ :Exception => null
			}
		}).asInstanceOf[AnyRef]



	@tailrec private def tracedPropertyName(mock :Any, res :StringBuilder = new StringBuilder) :String =
		if (mock == null)
			res.toString
		else {
			val traceField = try {
				mock.getClass.getField(TraceFieldName)
			} catch { //this method is used solely for the error message; we can't swallow the original exception
				case _ :Exception => null
			}
			if (traceField == null)
				res.toString
			else {
				val trace = traceField.get(mock).asInstanceOf[Trace]
				if (trace == null)
					res.toString
				else {
					if (res.nonEmpty)
						res += '.'
					tracedPropertyName(trace.owner, res ++= trace.method.getName)
				}
			}
		}






	private[this] val TracerCache :ConcurrentMap[Type, Class[_]] = ConcurrentTrieMap[Type, Class[_]]()
	private[this] val MockCache :ConcurrentMap[Class[_], Class[_]] = ConcurrentTrieMap[Class[_], Class[_]]()


	private val runtime = runtimeMirror(getClass.getClassLoader)
	private def classFor(tpe :Type) :Class[_] = runtime.runtimeClass(tpe.dealias.erasure.typeSymbol.asClass)

	private[this] val classLoader = new ByteArrayClassLoader(
		getClass.getClassLoader, false, java.util.Map.of[String, Array[Byte]]()
	)

	@inline private[this] final val TracerClassNameSuffix = "$$oldsql_tracer$$"
	@inline private[this] final val MockClassNameSuffix = "$$_oldsql_mock"
	@inline private[this] final val TraceFieldName = "__oldsql__trace__"
	@inline private[this] final val TypeMethodName = "__oldsql__type__"
	@inline private[this] final val BugInfo = "This is a bug!"

	@inline private[this] final val ClassSubstitutePropertyPrefix =
		"net.noresttherein.sugar.reflect.InvocationReflection.substitute."

	private[this] final val ClassSubstitutions = { //todo: better configuration
		var res = Map.empty[Class[_], Class[_]]
		val props = System.getProperties.keys()
		while (props.hasMoreElements) {
			val key = props.nextElement().asInstanceOf[String]
			if (key.startsWith(ClassSubstitutePropertyPrefix)) {
				val value = System.getProperty(key)
				val clazz = Class.forName(key.substring(ClassSubstitutePropertyPrefix.length))
				val substitute = Class.forName(value)
				res = res.updated(clazz, substitute)
			}
		}
		res.withDefault(identity)
//			classOf[Map[_, _]] -> classOf[DecomposableMap[_, _]],
//			classOf[Seq[_]] -> classOf[DecomposableSeq[_]]
	}

	@inline private[this] final val JavaByte    = classOf[j.Byte] :Any
	@inline private[this] final val JavaShort   = classOf[j.Short] :Any
	@inline private[this] final val JavaInteger = classOf[j.Integer] :Any
	@inline private[this] final val JavaLong    = classOf[j.Long] :Any
	@inline private[this] final val JavaFloat   = classOf[j.Float] :Any
	@inline private[this] final val JavaDouble  = classOf[j.Double] :Any
	@inline private[this] final val JavaChar    = classOf[j.Character] :Any
	@inline private[this] final val JavaBoolean = classOf[j.Boolean] :Any
	@inline private[this] final val ScalaUnit   = classOf[Unit] :Any
//	@inline private[this] final val ScalaOption = classOf[Option[Any]] :Any
	@inline private[this] final val JavaString  = classOf[String] :Any

	@inline private[this] final val ByteBuddy = new ByteBuddy

}

