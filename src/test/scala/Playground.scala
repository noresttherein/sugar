import net.noresttherein.slang.typist.InferTypeParams.Conforms


/**
  * @author Marcin Mościcki marcin@moscicki.net
  */
object Playground extends App {
//	type Tape
//
//	class ExtendedBy[+B <: Tape, -E <: Tape] private[Playground](val length :Int) extends AnyVal {
//		type Bound <: Tape
//		type Extension[+T <: Bound] <: Tape
//
//		def extension[T <: Bound] :T ExtendedBy Extension[T] = new ExtendedBy(length)
//	}

	class Bool(val value :Boolean)



}

	object StandardObject

	object SerializableObject extends Serializable

	case object CaseObject

    class Base(val field :Int) {
	    def canEqual(that :Any) :Boolean = that.isInstanceOf[Base]
	    override def equals(that :Any) :Boolean = that match {
		    case base :Base => base.field == field
		    case _ => false
	    }
	    override def hashCode :Int = field
    }

    case object BaseObject extends Base(42)