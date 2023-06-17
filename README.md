# Scala Sugar #
## 1. About ##
Sugar is a scala library focused on sugaring the syntax for commonly used code constructs and 
providing Scala-style interfaces to standard Java classes, but includes also various utility
types, including more lightweight implementations to those in the standard library.
It consists of mostly simple classes which would find use in most Scala projects - in fact 
you likely have developed some helper classes of similar functionality in the past more 
than once. They vary from simple facades for some standard Java libraries (namely, 
`java.time` and `java.logging`), through small utility classes and functions (like 
variable length tuples), to many one-liner methods and extension methods designed to make 
the intent behind code more apparent. It is directed more towards the implementation, than 
the interface side of things and, as such, a good candidate for a shaded dependency. 
In fact, as various pieces are almost completely independent and, more often than not, 
very small, it is easy to just copy&paste the bits you find useful to your own project, 
if you prefer to avoid the dependency altogether.

   

## 2. Contents ##

### 1. sugar.logging ###
A very simple `Logger` class wrapping the `java.util.logging.Logger` which accepts 
`msg: => Any` and correctly specifies its own caller as the logging class/method. 
If you declare your class logger as an implicit value, you can additionally use the
logging `Level` wrappers to log directly:
    
    Info(s"invoking method 'Cookie.fortune($seed)'")

This package is also home of the even simpler `debug` object providing methods for
impromptu debug logs which print argument values to various outputs before returning them:

    debug outln expression //a quick replacement for 'expression'
    debug(s"$x squared=") errln x*x      


   
### 2. sugar.time ###   
Almost one-to-one wrappers for classes representing time the from `java.time` package.
They are mostly value classes (meaning no wrapper objects are actually created unless you
take advantage of their polymorphic interfaces) which provide what you'd expect: 
temporal arithmetic using standard operators `+`,`-`,`*`, comparable with `<=` 
and friends, and generally more Scala-like method names (so, `date.month` instead of 
`date.getMonth`, etc.). There are some additional classes, providing support for 
infinite time spans (and infinitely remote time points) for interoperability with 
`scala.concurrent.duration`, and as a convenient solution for all 'wait indeterminately'
methods. The facade additionally provides some additional polymorphism (namely an 
abstraction over both date-oriented periods and time-oriented durations) and stronger
typing. Have a look at `sugar.time.Time` which serves as a general factory for
most classes to see what's available.

Naturally, you'll also find a DSL syntax for creating the values in `sugar.time.dsl`:

    10.seconds from now
    2.years + 3.months + 4.days before (11 Jan 1981)
    11:/11:/2011 at 11:-11     
   
   

### 3. sugar.exceptions ###
Extension methods 'scalafying' the API of `Throwable` (`Option` instead of `null`, Scala style properties).
Provides also a syntax for rethrowing exceptions with context information:

    rethrow { persistence.save(entity) } (s"Failed to save $entity.")

Extending `Rethrowable` reduces exception overhead, as it initializes its stack trace
based on the stack trace of its cause, using the suffix since the most recent `rethrow` call.



### 4. sugar.collections ###

#### 4.1. Many, many extension methods
Examples:

##### 4.1.1. Partial folding ####
    numbers.foldLeftWhile(0)(_ < Threshold)(_ + _)
    numbers.foldRightUntil(0)(_ >= Threshold)(_ + _)
    numbers.foldSome(0)((e, acc) => if (e + acc) < Threshold) Some(e + acc) else None)
and others.

##### 4.1.2. Mapping with state ####
    values.mapWithIndex{ (val, i) => s"$i: $val" }
    (Range(0, 10) zipMap (Range(11, 20))(_ + _)
    numbers.mapWith(0) { (val, sum) => (s" +$val = $sum", val + sum) }
and others.

##### 4.1.3. Fused zipping methods
Like lazy zip, but potentially with more efficient implementations and using multi-argument functions, 
which is good for lambda placeholder syntax:
     
    l.zipMapEven(r)(_ + _ * 2) //'even' methods throw an exception when collection sizes differ
    a.zipAll3(b, c, 'a', 'b', 'c') //like zipAll, but now does three collections for the price of two!

##### 4.1.4. Extension factory methods for standard `IterableFactory` objects
    Seq.generate(2) { case x if x <= 1024 => x }
    Iterator.over(Array(1, 2, 3))
    Stepper(singleton)
    Array.unfold(2)(Some(_ * 2).filter(_ <= 1024))

##### 4.1.5. `Seq` extensions
    Seq(1, 2, 3).isSorted
    dragons.getIndexOf(firkraag)                 // returns an Opt - non empty values are always >= 0
    dragons.sureIndexWhere(_.name == "Firkraag") //throws a NoSuchElementException instead of returning -1
    heroes.updatedAll(0, Seq("You", "Boo", "I")) //updated for consecutive elements
    weekdays.remove(0, 5)                        //an 'inverse' of slice
          
and others.

##### 4.1.6. Stepper, Iterator and Array extensions
Additional factory methods, in particular for specialized singleton implementations, but also bringing over
some omitted `java.util.Arrays` methods.


#### 4.2. 
New array types! All, including built in `Array` 'extending' from `ArrayLike`, and represented by non-wrapped `Array`s:

     IArray[+A]    //an immutable array - a taste of Scala 3 in Scala 2, and with more polymorphism, to boot
     RefArray[A]   //an Array[AnyRef], mascarading for an `Array[A]`, making the ubuiquitous practice properly type safe
     IRefArray[+A] //an Array[AnyRef] storing only values of `A`, for immutable use.

#### 4.3. PassedArray
An immutable version of a dope vector and a growing array buffer with O(1) *first* append/prepend.

#### 4.4. CubeBuffer 
A buffer implementation using multidimensional arrays for better memory usage characteristics.

#### 4.5. Jack
A universal, immutable sequence backed by a 'true' finger tree, with all operations taking `O(log n)` time,
except for access near the ends (`O(1)`) and methods requiring reading/updating every value in the sequence 
(`O(n)`, naturally).

#### 4.6. Ranking 
A collection of unique elements in a particular order - a `Set` and a `Seq` in one.

#### 4.7. MultiSet 
A collection aliasing repeating elements, with API for treating both as a standard `Iterable`
with repetitions, and as a `Map[A, Int]`.

#### 4.8. ZigZag 
A `Seq` offering O(1) append and prepend for any other `Seq`, at a cost of slower iteration,
designed for use as temporary buffers.

#### 4.9. ChoppedString 
A list-like collection of composed `String` concatenations
for use as an alternative to `StringBuilder` (has O(1) append/prepend, at the cost of O(n) random indexing).
Includes also a `Substring` subclass, useful in itself.

#### 4.10. NatMap and MutNatMap 
Maps `K[X] -> V[X]`, where `X` may be different for every entry.

#### 4.11. Other collections of marginal use ###
`EqSet`, `ConstSeq`, many `Stepper` and `Iterator` implementations.



### 5. sugar.optional ###
Syntax for creating options:

    x / y unless y == 0 //if (y!=0) Some(x / y) else None
    x + y providing y < Int.MaxValue - x
    numbers.flatMap(_ satisfying (_ > 0))
    className.indexOf('.') satisfying _ >= 0 
     


### 6. sugar.repeat ###
Various styles of `repeat` loop:

     repeat {
          sayBeep()
     } until (youAreMad())

     var genius = "bwa"
     val bwahaha = repeat {
          genius += "ha"; genius
     } until (_ == "bwahaha")

     val kilo = 1 repeat (_ * 2) until (_ > 1000) //1024
     val power = 1 count (_ * 2) until (_ > 1000) //10



### 7. sugar.numeric ###
  1. Extension methods for standard numeric value types, in particular bringing back static methods from 
     `java.lang.Integer` and the rest.
  2. `SafeInt` and `SafeLong` - overflow/underflow checking value types backed by `Int` and `Long`.
  3. `Ratio` and `IntRatio` - rational numbers implemented as pairs of values.
  4. `Decimal64` - a `BigDecimal`-like value class implemented on 64 bits, as per `java.math.MathContext.DECIMAL64`.



### 8. sugar.vars ###
Generic polymorphic `var` and `val` wrappers for use as in/out method parameters,
lazy/external initialization, garbage collecting handling and others.

    def testAndSet(i :InOut[Int]) :Unit = i.testAndSet(0, 1)
    val param    :InOut[Int] = Var(0)
    val sync     :InOut[Int] = SyncVar(0)    //synchronized access  
    val volatile :InOut[Int] = Volatile(0)   //declared as @volatile
    val atomic   :InOut[Int] = Atomic(0)     //uses memory fences
    val signal   :InOut[Int] = SignalVar(0)  //synchronized, notifies waiting threads on change 
    val watch    :InOut[Int] = Watched(0)    //@volatile var, observer pattern
    val thread   :InOut[Int] = ThreadLocal(0)

    val i :Val[Int] = Idempotent(1 + 1) //lazily evaluated, backed by a @volatile field (not synchronized)
    val t :Val[Int] = Transient(1+1)  //like above, value is @transient, initializer is serialized instead
    val u :Unsure[Int] = Sure(0)    //a @specialized Option substitute
    val o = Opt(null)             //a value class `Option` substitute    

#### 8.1. Var
A standard mutable value wrapper.

#### 8.2. SyncVar, Volatile, Atomic
Wrappers over mutable fields with synchronous access and varying memory semantics.

#### 8.3. SignalVal, SignalVar, Watched
Variables with API for waiting for value changes, or registering callbacks.

#### 8.4. Out
A thread safe variable which can be set - externally - only once.

#### 8.5. Freezer
A thread safe variable which can enter immutable state.

#### 8.6. Lazy, Idempotent, and Transient
Various alternatives of `lazy val`, providing information about initialization state
and alternative strategies for initialization for optimized contentious read.

#### 8.7. Relay
A synchronous thread communication chanel for passing individual messages through 
a value reader/writer handshake.

#### 8.8. Unsure, Opt, Potential
Optimized `Option` alternative implementations: `@specialized`, as a value class,
and a non boxing abstract type.

#### 8.9. Others
`EqRef`, `Box`, `Clearable`, `WeakRef`, `SoftRef`, `PhantomRef`, `ThreadLocal`, `Eval`, `EvalOpt`.

#### 8.10. Pill and Fallible
Alternatives to `Either` which do not box the 'right' (non error) values.



### 9. sugar.matching ###
#### 9.1. RX ####
A wrapper over `java.util.regex.Pattern` providing statically typed methods for all 
supported character classes and constructs - for those of us who can never remember
the more advanced features, or who value additional whitespace and commenting ability:

    val latinOrGreek = (Az | script"Greek") * //requires postfix calls enabled
    val id = Az::(AlNum|'_').*
    val line = `^` :: (!(`\\n`|`\\r`|`\\f`)).* :: `$`

The types, values and factory methods generally follow the Java flavour of regular expressions 
(so you'll find quite a few uses of quoted identifiers such as `^` and `$` above), 
which should make you feel at home with the character classes you know, and provide 
discovery and documentation viewing potential of a statically typed language in your IDE.
    
#### 9.2. RegexpGroupMatcher ####
Provides string interpolation syntax for regular expressions used in pattern matching:

    string match {
        case r"($user[^@]+)@($domain.+)" => println s"$user at $domain"
    }    
Arbitrary patterns can take place of the unbound variables `user` and `domain` in the above example.    

#### 9.3. && ####
The smallest but one of the most useful classes allows combining several extractor 
patterns in a logical conjunction:
    
    weapon match {
        case MeleeDmg(melee) && RangedDmg(ranged) => ...
    }

#### 9.4. Base traits and factories of match patterns and partial functions

    ints.collect(MatchFunction.collect { 
        (x :Int, default :Int => Int) => if (x % 2 == 0) x / 2 else default(x) 
    })
    val Green = MatchPattern { 
        x :Int => if ((x & ff00x) != 0) Some(x & ff00x >> 8) else None 
    }
    x match {
        case Green(greenChannel) => ???
    }



### 10. sugar.format ###
A framework for concise and non-error-prone defining of parser-formatters abstracting over the actual data formats:

    case class Dragon(name :String, color :String, level :Int)
    object Dragon {
        implicit val dragonFormat = for {
            dragon <- Format[Dragon]
            name   <- dragon("name")(_.name)
            color  <- dragon("color")(_.color)
            level  <- dragon("level")(_.level)
        } yield Dragon(name, color, level)
    }
    
    val xml = "<Dragon><name>Firkraag</name><color>Red</color><level>23</level></Dragon>"
    
    val dragon = xml as XML[Dragon]
    val json = dragon as JSON //"""{ 'name': "Firkraag", 'color': "Red", 'level': 23 }"""



### 11. sugar.util ###

#### 11.1. Decorable 
A pattern/framework for writing decorator aware classes, which do not 'lose' the decorator
when passing themselves to a callback method.

#### 11.2. LabelPath 
A type-level way of indexing objects by `String` literal types:

    record("weapon" / "damage" / "fireDamage") //may know the type of the accessed field



### 12. sugar.witness ###
Implicit values implementing `Not`, `Maybe` and `Or` over availability of other implicit values.
Also, tagging implicit values:

    type Planck
    type TidyPlanck
    implicit val h = Labeled[Planck](6.62607004d * math.pow(10, -34))
    implicit val tidy_h = Labeled[TidyPlanck](7d * math.pow(10, -34))
    implicitly[Double Labeled Planck]
    
    
    
### 13. sugar.typist ###
Safer casting methods - less powerful, imposing constraints on the source and target type,
including casting on type parameters for higher types.

    method(param).castFrom[ExpectedType, NewType] //this must be an ExpectedType to compile
    Seq(1, 2).castParam[java.lang.Integer]



### 14. sugar.reflect ###
  1. `PropertyPath`: reflecting a (possibly composite) properties given as getter functions:
      
         assert(PropertyPath(_.weapon.damage.fireDamange).toString == "weapon.damage.fireDamage")
  2. `Specialized` a type class carrying information about `@specialized` types, allowing to call specialized code
     from non-specialized, and providing separate callbacks for different value types.
  3. `RuntimeType`: an umbrella type class covering `ClassTag`, `TypeTag` and `Specialized` 
  4. `Class` extensions, in particular with methods such as `isBoxOf` dealing with the duality of boxed and unboxed 
     primitive values.

  5. Various ways for demangling and abbreviating class names for logging purposes, for example:

         object.getClass.name       //my.package.Singleton.Specialized_:[Int]
         object.getClass.abbrevName //m.p.Singleton.Specialized_:[Int]
         object.localClassName      //Singleton.Specialized_:[Int]
         object.innerClassName      //Specialized_:[Int]

Also, reflection-based utilities for implementing `toString` methods.



#### 15. sugar.Sealing ###
A pattern/utility class for expanding the function of `sealed` keyword to a package rather than a file,
and simulating `sealed` for methods (limiting not only visibility, but also the possibility of overriding).



### 16. others ###
Whose names are not worthy to appear here.    
