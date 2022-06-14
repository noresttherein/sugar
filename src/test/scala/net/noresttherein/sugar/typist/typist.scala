package net.noresttherein.sugar.typist



private class compileCheck_<:<[L <: U, A >: L <: U, B >: L <: U, U](implicit ev :A <:< B) {
	type CoLo[+_ >: L]
	type ContraLo[-_ >: L]
	type BiLo[-X >: L, +Y >: L]
	type CoUp[+_ <: U]
	type ContraUp[-_ <: U]
	type BiUp[-_ <: U, +_ <: U]
	type Co[+ _ >: L <: U]
	type Contra[-_ >: L <: U]
	type Bi[-_ >: L <: U, +_ >: L <: U]

	ev.lb[L].liftCo[CoLo]                       :CoLo[A] <:< CoLo[B]
	ev.lb[L].liftContra[ContraLo]               :ContraLo[B] <:< ContraLo[A]
	ev.lb[L].substituteCo(??? :CoLo[A])         :CoLo[B]
	ev.lb[L].substituteContra(??? :ContraLo[B]) :ContraLo[A]
	ev.lb[L].substituteBoth(??? :BiLo[B, A])    :BiLo[A, B]

	ev.ub[U].liftCo[CoUp]                       :CoUp[A] <:< CoUp[B]
	ev.ub[U].liftContra[ContraUp]               :ContraUp[B] <:< ContraUp[A]
	ev.ub[U].substituteCo(??? :CoUp[A])         :CoUp[B]
	ev.ub[U].substituteContra(??? :ContraUp[B]) :ContraUp[A]
	ev.ub[U].substituteBoth(??? :BiUp[B, A])    :BiUp[A, B]

//	ev.lb[L].ub[U].liftCo[Co]                       :Co[A] <:< Co[B]
//	ev.lb[L].ub[U].liftContra[Contra]               :Contra[B] <:< Contra[A]
//	ev.lb[L].ub[U].substituteCo(??? :Co[A])         :Co[B]
//	ev.lb[L].ub[U].substituteContra(??? :Contra[B]) :Contra[A]
//	ev.lb[L].ub[U].substituteBoth(??? :Bi[B, A])    :Bi[A, B]

	ev.ub[U].lb[L].liftCo[Co]                       :Co[A] <:< Co[B]
	ev.ub[U].lb[L].liftContra[Contra]               :Contra[B] <:< Contra[A]
	ev.ub[U].lb[L].substituteCo(??? :Co[A])         :Co[B]
	ev.ub[U].lb[L].substituteContra(??? :Contra[B]) :Contra[A]
	ev.ub[U].lb[L].substituteBoth(??? :Bi[B, A])    :Bi[A, B]
}



private class compileCheck_=:=[L <: U, A >: L <: U, B >: L <: U, U](implicit ev :A =:= B) {
	type UnLo[+_ >: L]
	type BiLo[X >: L, Y >: L]
	type UnUp[_ <: U]
	type BiUp[_ <: U, _ <: U]
	type Un[_ >: L <: U]
	type Bi[-_ >: L <: U, +_ >: L <: U]

	ev.lb[L].liftCo[UnLo]                       :UnLo[A] =:= UnLo[B]
	ev.lb[L].liftContra[UnLo]                   :UnLo[B] =:= UnLo[A]
	ev.lb[L].substituteCo(??? :UnLo[A])         :UnLo[B]
	ev.lb[L].substituteContra(??? :UnLo[B])     :UnLo[A]
	ev.lb[L].substituteBoth(??? :BiLo[B, A])    :BiLo[A, B]

	ev.ub[U].liftCo[UnUp]                       :UnUp[A] =:= UnUp[B]
	ev.ub[U].liftContra[UnUp]                   :UnUp[B] =:= UnUp[A]
	ev.ub[U].substituteCo(??? :UnUp[A])         :UnUp[B]
	ev.ub[U].substituteContra(??? :UnUp[B])     :UnUp[A]
	ev.ub[U].substituteBoth(??? :BiUp[B, A])    :BiUp[A, B]

//		ev.lb[L].ub[U].liftCo[Co]                       :Co[A] <:< Co[B]
//		ev.lb[L].ub[U].liftContra[Contra]               :Contra[B] <:< Contra[A]
//		ev.lb[L].ub[U].substituteCo(??? :Co[A])         :Co[B]
//		ev.lb[L].ub[U].substituteContra(??? :Contra[B]) :Contra[A]
//		ev.lb[L].ub[U].substituteBoth(??? :Bi[B, A])    :Bi[A, B]

	ev.ub[U].lb[L].liftCo[Un]                       :Un[A] =:= Un[B]
	ev.ub[U].lb[L].liftContra[Un]                   :Un[B] =:= Un[A]
	ev.ub[U].lb[L].substituteCo(??? :Un[A])         :Un[B]
	ev.ub[U].lb[L].substituteContra(??? :Un[B])     :Un[A]
	ev.ub[U].lb[L].substituteBoth(??? :Bi[B, A])    :Bi[A, B]
}
