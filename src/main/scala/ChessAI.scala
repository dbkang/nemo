import PositionValueOrdered.fromPositionValue
import Square.fromInt
import BitBoard.fromLong

trait PositionValue

case class ForcedMate(val plies:Int, val win:Boolean) extends PositionValue

case object Draw extends PositionValue


case class WinProbability(val p:Double) extends PositionValue

case class PieceVal(val v:Double) extends PositionValue


//TODO: Use WinProbability instead of PieceVal eventually
final class PositionValueOrdered(val self:PositionValue) extends Ordered[PositionValue] {
  def compare(that:PositionValue) = {
    (self, that) match {
      case (ForcedMate(_,true), Draw) => 1
      case (ForcedMate(_,true), PieceVal(_)) => 1
      case (ForcedMate(n1,true),ForcedMate(n2,true)) => n2 - n1
      case (ForcedMate(_,true), ForcedMate(_,false)) => 1
      case (PieceVal(v), Draw) => v.signum
      case (PieceVal(v1), PieceVal(v2)) => (v1 - v2).signum
      case (PieceVal(_), ForcedMate(_,false)) => 1
      case (Draw, Draw) => 0
      case (Draw, ForcedMate(_, false)) => 1
      case (ForcedMate(n1, false), ForcedMate(n2, false)) => n1 - n2
      case (a, b) => - (b compare a)
    }
  }
}

object PositionValueOrdered {
  implicit def fromPositionValue(p:PositionValue):PositionValueOrdered = new PositionValueOrdered(p)
}

// giving PositionValue some numeric tools

object PositionValue {
  val max = ForcedMate(0,true)
  val min = ForcedMate(0,false)
  def minus(p:PositionValue):PositionValue = {
    p match {
      case ForcedMate(p, w) => ForcedMate(p, !w)
      case Draw => Draw
      case WinProbability(p) => WinProbability(1-p)
      case PieceVal(v) => PieceVal(-v)
    }
  }
}

// With principal variation
case class PositionValuePV(val v:PositionValue, val pv:Seq[ChessMovePosition]) extends Ordered[PositionValuePV] {
  def compare(that:PositionValuePV) = v compare that.v
  def unary_- = PositionValuePV(PositionValue.minus(v), pv)
}

object PositionValuePV {
  val min = PositionValuePV(PositionValue.min,Seq[ChessMovePosition]())
  val max = PositionValuePV(PositionValue.max,Seq[ChessMovePosition]())
}

class ChessAI extends ChessPlayer {
  var color:SideColor = White
  var position:Position = Position.starting
  var lastPV:PositionValuePV = PositionValuePV(Draw, Seq[ChessMovePosition]())
  var numPositionsSearched = 0

  def makeMove(m:ChessMove):Unit = {
    position = position.makeMove(m)
  }

  def nextMove:ChessMove = {
    import scala.compat.Platform.currentTime
    val startingTime = currentTime
    val numSearchedPre = numPositionsSearched
    println("Starting search at " + startingTime )
    val depth = 3
    for (i <- 1 to depth - 1) {
      alphabeta(position, i, PositionValuePV.min, PositionValuePV.max, Seq[ChessMovePosition]())
    }
    val best = alphabeta(position, depth, PositionValuePV.min, PositionValuePV.max, Seq[ChessMovePosition]())
    lastPV = best
    val endingTime = currentTime
    println("Finished search at " + endingTime + ".")
    println("Took " + (endingTime - startingTime) + "ms" )
    println("Searched " + (numPositionsSearched - numSearchedPre) + " positions.")
    best.pv(0).move
  }

    
  // TODO: Make it more reasonable
  // TODO: as a preliminary step, incorporate mobility
  def eval(p:Position, me:SideColor):PositionValue = {
    numPositionsSearched = numPositionsSearched + 1
    if (p.isKingInCheck(p.turn) && p.legalMoves.length == 0)
      ForcedMate(0,false)
    else if (p.legalMoves.length == 0)
      Draw
    else
      PieceVal(
        1.0 * (p.bbPiece(Pawn(me)).count - p.bbPiece(Pawn(Opp(me))).count) +
        3.0 * (p.bbPiece(Knight(me)).count - p.bbPiece(Knight(Opp(me))).count) +
        3.0 * (p.bbPiece(Bishop(me)).count - p.bbPiece(Bishop(Opp(me))).count) +
        5.0 * (p.bbPiece(Rook(me)).count - p.bbPiece(Rook(Opp(me))).count) +
        9.0 * (p.bbPiece(Queen(me)).count - p.bbPiece(Queen(Opp(me))).count) +
        0.1 * (p.influenceCount - p.influenceCountOpp))
  }

  // ForcedMate(n) => ForcedMate(n+1), otherwise return as is
  private def addDepth(pv:PositionValuePV) = {
    pv match {
      case PositionValuePV(ForcedMate(n,w), l) => PositionValuePV(ForcedMate(n+1,w),l)
      case _ => pv
    }
  }        

  def alphabeta(p:Position,d:Int, a:PositionValuePV, b:PositionValuePV, pv:Seq[ChessMovePosition]):PositionValuePV = {
    val moves = p.legalMoves
    val value = {
      if (d == 0 || moves.length == 0)
        PositionValuePV(eval(p, p.turn), pv)
      else {
        var max = a
//        for (m <- moves.sortBy(ma => PositionValue.minus(p.makeMove(ma).value)) if b > max) {
        for (m <- moves.sortBy(ma => p.makeMove(ma).value) if b > max) {
          val newPV = -addDepth(alphabeta(p.makeMove(m), d - 1, -b, -a, pv :+ ChessMovePosition(p,m)))
          if (newPV >= max)
            max = newPV
        }
        max
      }
    }
    if (d >= p.valueDepth) {
      p.value = value.v
      p.valueDepth = d
    }
    value
  }
}

