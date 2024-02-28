package net.noresttherein.sugar.arrays;

import scala.collection.immutable.Vector;
import scala.collection.mutable.ArrayBuffer;

class CheatedAccess {
    static final int FlatVectorSize = 32;
    static <T> Object[] array(ArrayBuffer<T> buffer) {
        return buffer.array();
    }
    static <T> Object[] array(Vector<T> vector) { return vector.prefix1(); }
}