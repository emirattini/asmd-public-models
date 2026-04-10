package scala.u06.examples

export u06.modelling.PetriNet
import u06.utils.MSet

object PNReadersWriters:

  enum Place:
    case p1, p2, p3, p4, p5, p6, p7
    
  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  // DSL-like specification of a Petri Net
  def readersWriters = PetriNet[Place](
    (p1, p2) ~~> p3,
    (p1, p3, p3) ~~> p3
  ).toSystem
@main def mainPNReadersWriters = {
  import PNReadersWriters.*
  // example usage
  println(readersWriters.paths(MSet(p1,p1),7).toList.mkString("\n"))
}
