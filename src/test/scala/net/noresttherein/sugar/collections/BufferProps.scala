package net.noresttherein.sugar.collections

import scala.collection.immutable.AbstractSeq
import scala.collection.mutable.{Buffer, Builder}
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.{Failure, Success}

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.{arbitrary => gen}
import org.scalacheck.Gen.const
import org.scalacheck.commands.Commands
import org.scalacheck.util.{Buildable, ConsoleReporter}
import net.noresttherein.sugar.extensions.{BooleanExtension, ClassExtension, ThrowableExtension}
import net.noresttherein.sugar.testing.scalacheck.extensions.{BooleanAsPropExtension, LazyExtension, PropExtension}



//The easiest way to test buffers is to temporarily implement toString
// so that it shows offset..(offset+length)/array.length.
// Look at scalacheck action list output and search for the last successful one, noting the buffer stats.
// Then simply create a buffer with the specified initial size, add offset+length elements, and drop offset elements.
// This way you can then just debug the last action, applying it to the recreated buffer.

trait BufferProps[C[X] <: Buffer[X], E[_]] extends Properties { //extends SeqProps[C, E] {
	protected def minSuccessfulTests = 500
	protected def maxSize = 64

	override def overrideParameters(p :Test.Parameters) :Test.Parameters =
		p.withTestCallback(ConsoleReporter(3, 140)).withMinSuccessfulTests(minSuccessfulTests).withMaxSize(maxSize)

	val parameters = overrideParameters(Test.Parameters.default)

	type Buff[X] = C[X]

	implicit protected def intEvidence :E[Int]

	implicit def buildableChecked[T :E] :Buildable[T, C[T]]
	import net.noresttherein.sugar.testing.scalacheck.noShrinking


	implicit def arbitrary[T :E :Arbitrary] :Arbitrary[C[T]] = Arbitrary(
		for {
			len  <- Gen.choose(0, parameters.maxSize)
			drop <- Gen.choose(0, len)
			buff <- Gen.containerOfN[Buff, T](len, Arbitrary.arbitrary[T])
		} yield buff.dropInPlace(drop)
	)

	property("apply") = forAll { seq :IndexedSeq[Int] =>
		val buffer = (buildableChecked[Int].builder ++= seq).result()
		all(0 until buffer.length map { i => seq(i) =? buffer(i) lbl s"apply($i)" } :_*) &&
			(buffer(-1).throws[IndexOutOfBoundsException] lbl "apply(-1)") &&
			(buffer(buffer.length).throws[IndexOutOfBoundsException] lbl s"apply(length==${buffer.length})") lbl
			buffer.toString
	}


	class BufferCommands[X :E :Arbitrary :ClassTag](init :Seq[X] => C[X]) extends Commands {
		override type State = Either[Class[_], Seq[X]]
		override type Sut = C[X]

		override def canCreateNewSut(newState :State, initSuts :Iterable[State], runningSuts :Iterable[C[X]]) :Boolean =
			true

		override def newSut(state :State) :C[X] = init(state.asInstanceOf[Right[Class[_], Seq[X]]].value)
		override def destroySut(sut :C[X]) :Unit = ()

		override def initialPreCondition(state :State) :Boolean = state.isRight

		override def genInitialState :Gen[State] =
			Gen.containerOf[SizedSeq, X](Arbitrary.arbitrary[X]).map(Right(_))

		protected def indexGen(length :Int) = //Gen.choose(-length - 16, 2 * length + 16)
			Gen.choose(-1, length + 1)

		override def genCommand(state :State) :Gen[Command] = state match {
			case Left(_)    => Gen.const(Clear)
			case Right(seq) =>
				val length = seq.length
				Gen.oneOf(
					indexGen(length).flatMap(i => gen[X].map(x => Update(i, x))),
					indexGen(length).map(Remove.apply),
					indexGen(length).flatMap(i => indexGen(length - i).map(count => RemoveAll(i, count))),
					indexGen(length).flatMap(i => gen[X].map(x => Insert(i, x))),
					indexGen(length).flatMap(i => gen[SizedSeq[X]].map(xs => InsertAll(i, xs))),
//					const(Clear), //for some reason scalacheck gives up with this enabled.
					gen[X].map(AddOne),
					gen[SizedSeq[X]].map(AddAll),
					gen[X].map(Prepend),
					gen[SizedSeq[X]].map(PrependAll),
					for { idx <- indexGen(length); col <- gen[SizedSeq[X]]; replaced <- indexGen(length) }
						yield Patch(idx, replaced, col)
				)
		}


		trait BufferCommand extends Command {
			override type Result = Seq[X]
			def thrown :PartialFunction[collection.Seq[X], Class[_]] = PartialFunction.empty

			override def preCondition(state :State) = state.isRight

			override def postCondition(state :State, result :Try[Result]) =
				(nextState(state), result) match {
					case (Right(left), Success(right)) =>
						(left =? right) && all(
							left.indices.map(i => left(i) =? right(i) lbl s"apply($i)")
						: _*) &&
							(right(-1).throws[IndexOutOfBoundsException] :| "apply(-1)") &&
							(right(left.length).throws[IndexOutOfBoundsException] :| s"apply(${left.length})")
					case (Left(cls), Failure(exception)) =>
						cls.isInstance(exception) lbl
							"Should have thrown " + cls.localName + ", but threw " + exception + "\n" +
								exception.stackTraceString
					case (Left(cls), Success(right)) =>
						Prop.falsified lbl "Should have thrown " + cls.localName + ", but returned " + right
					case (Right(left), Failure(e)) =>
						Prop.falsified lbl "Expected " + left + ",\nbut threw " + e + "\n" + e.stackTraceString
				}

			override def run(sut :Sut) :Seq[X] = {
				apply(sut)
				new SeqWithBuffer(sut)
			}
			class SeqWithBuffer(buffer :Buffer[X]) extends AbstractSeq[X] {
				val result = buffer.toVector
				override def knownSize :Int = result.knownSize
				override def length :Int = result.length
				override def apply(i :Int) :X = result(i)
				override def iterator :Iterator[X] = result.iterator
				override def toString :String = buffer.toString + " (" + result + ")"
			}
			override def nextState(state :State) :State = state.flatMap { input =>
				try thrown.lift(input).map(Left(_)) getOrElse Right(apply(input)) catch {
					case _ :IndexOutOfBoundsException => Left(classOf[IndexOutOfBoundsException])
					case _ :IllegalArgumentException  => Left(classOf[IllegalArgumentException])
				}
			}

			protected def apply(sut :Sut) :Unit
			protected def apply(state :Seq[X]) :Seq[X]

		}


		abstract class IndexedCommand(idx :Int, inclusive :Boolean = false) extends BufferCommand {
			override def thrown = {
				case seq :collection.Seq[X] if idx < 0 || seq.length - 1 + inclusive.toInt < idx =>
					classOf[IndexOutOfBoundsException]
			}
		}

		def mutate(name: => String)(result :Sut => Unit, expect :Seq[X] => Seq[X]) :Command =
			new BufferCommand {
				override def apply(sut :Sut) = result(sut)
				override def apply(state :Seq[X]) = expect(state)
				override def toString = name
			}
		def mutate(name: => String, index :Int, inclusive :Boolean = false)
		          (result :Sut => Unit, expect :Seq[X] => Seq[X]) :Command =
			new IndexedCommand(index, inclusive) {
				override def apply(sut :Sut) = result(sut)
				override def apply(state :Seq[X]) = expect(state)
				override def toString = name
			}


		case class Update(index :Int, value :X) extends IndexedCommand(index) {
			override def apply(sut :Sut) = sut(index) = value
			override def apply(state :Seq[X]) = state.updated(index, value)
		}
//		case class UpdateAll(index :Int, elems :Seq[X]) extends IndexedCommand(index) {
//			override def apply(sut :Sut) = sut.updateAll
//		}

		def AddOne(elem :X) = mutate(s"AddOne($elem)")(_.addOne(elem), _.appended(elem))
		def AddAll(elems :Iterable[X]) = mutate(s"AddAll($elems)")(_.addAll(elems), _.appendedAll(elems))

		def Prepend(elem :X) = mutate(s"Prepend($elem)")(_.prepend(elem), _.prepended(elem))
		def PrependAll(elems :Iterable[X]) =
			mutate(s"PrependAll($elems)")(_.prependAll(elems), _.prependedAll(elems))

		case class Remove(index :Int) extends IndexedCommand(index) {
			//We don't check if the value returned by remove is correct, but it would require State
			// to include some Result case classes
			override def apply(sut :Sut) = sut.remove(index)
			override def apply(state :Seq[X]) = state.iterator.take(index) ++ state.iterator.drop(index + 1) to Seq
		}

		case class RemoveAll(index :Int, count :Int) extends BufferCommand {
			override def thrown = {
				case _ :collection.Seq[X] if count < 0 => classOf[IllegalArgumentException]
				case seq :collection.Seq[X] if count > 0 && (index < 0 || count > seq.length - index) =>
					classOf[IndexOutOfBoundsException]
			}
			override def apply(sut :Sut) = sut.remove(index, count)
			override def apply(state :Seq[X]) :Seq[X] =
				state.iterator.take(index) ++ state.iterator.drop(index).drop(count) to Seq
		}

		case class Insert(index :Int, elem :X) extends IndexedCommand(index, true) {
			override def apply(sut :Sut) = sut.insert(index, elem)
			override def apply(state :Seq[X]) :Seq[X] =
				state.iterator.take(index) ++ Iterator.single(elem) ++ state.iterator.drop(index) to Seq
		}

		case class InsertAll(index :Int, elems :Iterable[X]) extends IndexedCommand(index, true) {
			override def apply(sut :Sut) = sut.insertAll(index, elems)
			override def apply(state :Seq[X]) =
				state.iterator.take(index) ++ elems.iterator ++ state.iterator.drop(index) to Seq
		}

		case class Patch(index :Int, replaced :Int, elems :Iterable[X]) extends BufferCommand {
			override def apply(sut :Sut) = sut.patchInPlace(index, elems, replaced)
			override def apply(state :Seq[X]) :Seq[X] = state.patch(index, elems, replaced)
		}

		case object Clear extends BufferCommand {
			override def apply(sut :Sut) :Unit = sut.clear()
			override def apply(state :Seq[X]) = Vector.empty
		}
	}

}



//class UntaggedBufferProps[C[X] <: Buffer[X]](name :String, factory :IterableFactory[C])
//	extends UntaggedSeqProps[C](name, factory) with BufferProps[C, Dummy]
