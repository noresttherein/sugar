package net.noresttherein.sugar.collections;

import scala.Function1;

class IterableNameAccessor implements Function1<scala.collection.Iterable<?>, String> {
	public String apply(scala.collection.Iterable<?> elems) { return elems.collectionClassName(); }
}