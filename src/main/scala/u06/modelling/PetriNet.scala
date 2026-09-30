package u06.modelling

import u06.utils.MSet

object PetriNet:
  // pre-conditions, effects, inhibition
  case class Trn[P](cond: MSet[P], eff: MSet[P], inh: MSet[P])
  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  // factory of A Petri Net
  def apply[P](transitions: Trn[P]*): PetriNet[P] = transitions.toSet

  // factory of a System, as a toSystem method
  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      for
        Trn(cond, eff, inh) <- pn   // get any transition
        if m disjoined inh          // check inhibition
        out <- m extract cond       // remove precondition
      yield out union eff           // add effect
  
  // anything that can stand on a side of ~~>: a place, a tuple of places, a marking
  trait AsMarking[X, P]:
    def apply(x: X): Marking[P]
  object AsMarking extends LowPriorityAsMarking:
    given tuple[P, T <: Tuple](using Tuple.Union[T] <:< P): AsMarking[T, P] =
      MSet.ofTuple(_)
    given marking[P]: AsMarking[Marking[P], P] = identity(_)
  trait LowPriorityAsMarking:
    given single[P]: AsMarking[P, P] = MSet(_)

  // fancy syntax to create transition rules
  extension [X](self: X)
    def ~~> [Y, P](y: Y)(using l: AsMarking[X, P], r: AsMarking[Y, P]): Trn[P] =
      Trn(l(self), r(y), MSet())
  extension [P](self: Trn[P])
    def ^^^ (z: Marking[P]): Trn[P] = self.copy(inh = z)
    def ^^^ [Z](z: Z)(using r: AsMarking[Z, P]): Trn[P] = self.copy(inh = r(z))