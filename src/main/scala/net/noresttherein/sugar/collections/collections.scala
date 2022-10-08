package net.noresttherein.sugar

import java.lang.reflect.Field
import java.util.PrimitiveIterator

import scala.collection.{Factory, IterableFactory, MapFactory}

import net.noresttherein.sugar.vars.Opt
import net.noresttherein.sugar.vars.Opt.{Got, Lack}

//implicits
import net.noresttherein.sugar.extensions.optionExtension




/**
 * @author Marcin Mościcki
 */
package object collections {
	final val ver = 1L

	final val ElemTypes = Specializable.AllNumeric

	type JavaIntIterator    = PrimitiveIterator.OfInt
	type JavaLongIterator   = PrimitiveIterator.OfLong
	type JavaDoubleIterator = PrimitiveIterator.OfDouble


	//todo: MultiSet
	//todo: compare also factories with implicit evidence
	private val IterableToFactoryClass = scala.collection.Iterable.iterableFactory.getClass
	private val iterableFactoryField :Opt[Field] =
		IterableToFactoryClass.getFields.find(_.getType == classOf[IterableFactory[Iterable]]).toOpt
	private val MapToFactoryClass = scala.collection.Map.mapFactory.getClass
	private val mapFactoryField :Opt[Field] =
		MapToFactoryClass.getFields.find(_.getType == classOf[MapFactory[Map]]).toOpt

	private[sugar] def companionFactoryOf[E, T](factory :Factory[E, T]) :Opt[Any] =
		factory match {
			case comparable :ComparableFactory[_, _] => Got(comparable.factory)
			case _ if IterableToFactoryClass isAssignableFrom factory.getClass =>
				iterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[Iterable]])
			case _ if MapToFactoryClass isAssignableFrom factory.getClass =>
				mapFactoryField.map(_.get(factory).asInstanceOf[MapFactory[Map]])
			case _ => Lack
		}

}
