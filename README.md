# slang scala #
## 1. About ##
Slang is a scala library focused on sugaring the syntax for commonly used code constructs.
It consists of mostly simple classes which would find use in most scala projects - in fact 
you likely have developed some helper classes of similar functionality in the past more 
than once. They vary from simple facades for some standard Java libraries (namely, 
`java.time` and `java.logging`), through small utility classes and functions (like 
variable length tuples), to many one-liner methods and extension methods designed to make 
the intent behind code more apparent. It is directed more towards the implementation than 
the interface side of things and, as such, a good candidate for a shaded dependency. 
In fact, as various pieces are almost completely independent and more often than not 
very small, it is easy to just copy&paste the bits you find useful to your own project, 
if you prefer to avoid the dependency altogether.

   

## 2. Contents ##

### 1. slang.logging ###
A very simple `Logger` class wrapping the `java.util.logging.Logger` which accepts 
`msg: => Any` and correctly specifies its own caller as the logging class/method. 
If you declare your class logger as an implicit value, you can additionally use the
logging `Level` wrappers to log directly:
    
    Info(s"invoking method 'Cookie.fortune($seed)'")

This package is also home of the even simpler `debug` object providing methods for
impromptu debug logs which print argument values to various outputs before returning them:

    debug outln expression //a quick replacement for 'expression'
    debug(s"$x squared=") errln x*x      


   
### 2. slang.time ###   
Almost one-to-one wrappers for classes representing time the from `java.time` package.
They are mostly value classes (meaning no wrapper objects are actually created unless you
take advantage of their polymorphic interfaces) which provide what you'd expect: 
temporal arithmetic using standard operators `+`,`-`,`*`, comparable with `<=` 
and friends and generally more scala-like method names (so, `date.month` instead of 
`date.getMonth`, etc.). There are some additional classes, providing support for 
infinite time spans (and infinitely remote time points) for interoperability with 
`scala.concurrent.duration` and as a convenient solution for all 'wait indeterminately'
methods. The facade additionally provides some additional polymorphism (namely an 
abstraction over both date-oriented periods and time-oriented durations) and stronger
typing. Have a look at `slang.time.Time` which serves as a general factory for
most classes to see what's available.

Naturally, you'll also find a DSL syntax for creating the values in `slang.time.dsl`:

    10.seconds from now
    2.years + 3.months + 4.days before (11 Jan 1981)
    11:/11:/2011 at 11:-11     
   
   

### 3. slang.exceptions ###
Extension methods 'scalafying' the API of `Throwable` (`Option` instead of `null`, Scala style properties).
Provides also a syntax for rethrowing exceptions with context information:

    rethrow { persistence.save(entity) (s"Failed to save $entity.")

Extending `Rethrowable` reduces exception overhead, as it initializes its stack trace
based on the stack trace of its cause, using the suffix since most recent `rethrow` call.



### 4. slang.collection ###
Extension methods for collections, including:

#### 4.1 Partial folding ####
    numbers.foldLeftWhile(0)(_ < Threshold)(_ + _)
    numbers.foldRightUntil(0)(_ >= Threshold)(_ + _)
    numbers.foldSome(0)((e, acc) => if (e + acc) < Threshold) Some(e + acc) else None)
and others.

#### 4.2 Mapping with state ####
    values.mapWithIndex{ (val, i) => s"$i: $val" }
    numbers.mapWith(0) { (val, sum) => s" +$val = $sum" }

Also:
1. `ArrayIterator` and `ReverseArrayIterator`
2. `ChunkedString` - a list-like collection of composed `String` concatentations
   for use as an alternative to `StringBuilder` (has O(1) append/prepend, at the cost of O(n) random indexing).



### 5. slang.optional ###
Syntax for creating options:

    x / y unless y == 0 //if (y!=0) Some(x / y) else None
    x + y providing y < Int.MaxValue - x
    numbers.flatMap(_ satisfying (_ > 0))
    className.indexOf('.') satisfying _ >= 0 



### 6. slang.repeat ###
Various styles of `repeat` loop:

    repeat {
        sayBeep()
    } until (youAreMad())

    var genius = "bwa"
    val bwahaha = repeat {
        genius += "ha"; genius
    } until (_ == "bwahaha")

    val kilo = 2 repeat (_ * 2) until (_ > 1000)


### 7. slang.numeric ###
  1. `SafeInt` and `SafeLong` - overflow/underflow checking value types backed by `Int` and `Long`.
  2. `Ratio` and `IntRatio` - rational numbers implemented as pairs of values.
  3. `Decimal64` - a `BigDecimal` value class implemented on 64 bits, as per `java.math.MathContext.DECIMAL64`.
  4. `Decimal[N]` - a fixed precision decimal number with up to `N <: Int with Singleton` fractional digits.



### 8. slang.vars ###
Generic polymorphic `var` and `val` wrappers for use as in/out method parameters:

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

Plus quite a few more, like hand-shake multi-threaded channel `Relay[T]`.
Naturally, you'll find all the arithmetic operations you'd expect such as `+=` and `++`, as well as
a couple more.



### 9. slang.matching ###
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
    
#### 9.2 RegexpGroupMatcher ####
Provides string interpolation syntax for regular expressions used in pattern matching:

    string match {
        case r"($user[^@]+)@($domain.+)" => println s"$user at $domain"
    }    
Arbitrary patterns can take place of the unbound variables `user` and `domain` in the above example.    

#### 9.3 && ####
The smallest but one of the most useful classes allows combining several extractor 
patterns in a logical conjunction:
    
    weapon match {
        case MeleeDmg(melee) && RangedDmg(ranged) => ...
    }



### 10. slang.prettyprint ###
Various ways for demangling and abbreviating class names for logging purposes, for example:

    object.getClass.name       //my.package.Singleton.Specialized_:[Int]
    object.getClass.abbrevName //m.p.Singleton.Specialized_:[Int]
    object.localClassName      //Singleton.Specialized_:[Int]
    object.innerClassName      //Specialized_:[Int]

Also, reflection-based utilities for implementing `toString` methods.



### 11. slang.witness ###
Implicit values implementing `Not`, `Maybe` and `Or` over availability of other implicit values.
Also, tagging implicit values:

    val Planck = Labelled.Label
    val TidyPlanck = Labelled.Label
    implicit val h = Label[Planck.@#](6.62607004d * math.pow(10, -34))
    implicit val tidy_h = Label[TidyPlanck.@#](7d * math.pow(10, -34))
    implicitly[Double Labelled Planck.@#]
    
    
    
### 12. slang.typist ###
Safer casting methods - less powerful, imposing constraints on the source and target type,
including casting on type parameters for higher types.



### 13. slang.Sealing ###
A pattern/utility class for expanding the function of `sealed` keyword to a package rather than a file,
and simulating `sealed` for methods (limiting not only visibility, but also the possibility of overriding).



### 14. others ###
Whose names are not worthy to appear here.    
