package net.noresttherein.sugar.arrays;

import net.noresttherein.sugar.collections.CountdownIterator;



/** This class is '''not''' a part of public API. Internal use only. */
abstract class MatrixIteratorState {
	protected final int Dim1;
	protected int idx2;
	protected int idx1;
	protected int countdown;

	MatrixIteratorState(int length1, int idx2, int idx1, int countdown) {
		this.Dim1 = length1;
		this.idx2 = idx2;
		this.idx1 = idx1;
		this.countdown = countdown;
	}

	protected abstract MatrixIteratorState setCurr();
}


/** This class is '''not''' a part of public API. Internal use only. */
abstract class CuboidIteratorState {
	protected final int Dim1;
	protected final int Dim2;
	protected int idx3;
	protected int idx2;
	protected int idx1;
	protected int countdown;

	CuboidIteratorState(int length2, int length1, int idx3, int idx2, int idx1, int countdown) {
		this.Dim1 = length1;
		this.Dim2 = length2;
		this.idx1 = idx1;
		this.idx2 = idx2;
		this.idx3 = idx3;
		this.countdown = countdown;
	}

	protected abstract CuboidIteratorState setCurr();

//	protected final int remaining() { return countdown; }
}
