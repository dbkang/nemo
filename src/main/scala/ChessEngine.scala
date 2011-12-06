import BitBoard.fromLong
import Square.fromInt


object Square {
  def apply(file:Int,rank:Int):Int = file * 8 + rank
  def fromAlgebraic(a:String):Int = apply(a.charAt(0).toUpper-'A',a.charAt(1)-'1')
  def apply(a:String):Int = fromAlgebraic(a)
  def apply(l:Seq[String]):Seq[Int]=l.map(fromAlgebraic(_))
  implicit def fromInt(v:Int) = new Square(v)
  def apply(v:Int) = fromInt(v)
  //implicit def toInt(v:Square) = new Square(v)
}

trait ChessMove {
  def piece: Piece
  def squareFrom: Int
  def squareTo: Int
  override def toString = squareFrom.toAlgebraic + " " + squareTo.toAlgebraic
}


class Square(val self: Int) {
  def toAlgebraic:String = ('a' + file).toChar.toString + (rank + 1).toString
  def file = self / 8
  def rank = self % 8
}

trait SideColor {
  def isBlack: Boolean
  def isWhite: Boolean
}

case object Black extends SideColor { 
  val isBlack = true
  val isWhite = false
}

case object White extends SideColor {
  val isBlack = false
  val isWhite = true
}

object Opp {
  def apply(side:SideColor) = side match {
    case Black => White
    case White => Black
  }
}

trait Piece {
  def side:SideColor
}

case class EmptySquare(side: SideColor) extends Piece

case class Knight(side:SideColor) extends Piece
case class Pawn(side:SideColor) extends Piece
case class Bishop(side:SideColor) extends Piece
case class Rook(side:SideColor) extends Piece
case class King(side:SideColor) extends Piece
case class Queen(side:SideColor) extends Piece


case class NormalMove(piece:Piece, squareFrom:Int, squareTo:Int) extends ChessMove

case class CastleMove(piece:King, kingside:Boolean) extends ChessMove {
  def squareFrom = piece match {
    case King(White) => Square.fromAlgebraic("E1")
    case King(Black) => Square.fromAlgebraic("E8")
  }
  def squareTo = (piece, kingside) match {
    case (King(White), true) => Square.fromAlgebraic("G1")
    case (King(White), false) => Square.fromAlgebraic("C1")
    case (King(Black), true) => Square.fromAlgebraic("G8")
    case (King(Black), false) => Square.fromAlgebraic("C8")
  }
}

case class PawnSingleMove(piece:Pawn, squareFrom:Int) extends ChessMove {
  def squareTo = piece match {
    case Pawn(White) => Square(squareFrom.file, squareFrom.rank + 1)
    case Pawn(Black) => Square(squareFrom.file, squareFrom.rank - 1)
  }
}


case class PawnDoubleMove(piece:Pawn, squareFrom:Int) extends ChessMove {
  def squareTo = piece match {
    case Pawn(White) => Square(squareFrom.file, squareFrom.rank + 2)
    case Pawn(Black) => Square(squareFrom.file, squareFrom.rank - 2)
  }
}

// includes en passant capture
case class PawnCaptureMove(piece:Pawn, squareFrom:Int, squareTo:Int) extends ChessMove
case class PawnCapturePromotionMove(piece:Pawn, squareFrom:Int, squareTo:Int, newPiece:Piece) extends ChessMove

case class PawnPromotionMove(piece:Pawn, squareFrom:Int, newPiece:Piece) extends ChessMove {
  def squareTo = piece match {
    case Pawn(White) => Square(squareFrom.file, squareFrom.rank + 1)
    case Pawn(Black) => Square(squareFrom.file, squareFrom.rank - 1)
  }
}

//case class KnightMove(piece:Knight, squareFrom:Square, squareTo:Square) extends ChessMove


// these have to be handled specially because of castling implications
case class RookMove(piece:Rook, squareFrom:Int, squareTo:Int) extends ChessMove {
  def side = piece match {
    case Rook(Black) => {
      if (squareFrom == Square.fromAlgebraic("A8"))
        Some(Queen(Black))
      else if (squareFrom == Square.fromAlgebraic("H8"))
        Some(King(Black))
      else
        None
    }
    case Rook(White) => {
      if (squareFrom == Square.fromAlgebraic("A1"))
        Some(Queen(White))
      else if (squareFrom == Square.fromAlgebraic("H1"))
        Some(King(White))
      else
        None
    }
  }
}

case class KingMove(piece:King, squareFrom:Int, squareTo:Int) extends ChessMove 

object Position {
  def bbFromAlgSeq(posList:Seq[String]) = BitBoard.fromSeqOfBits(posList.map(Square.fromAlgebraic _))
  def starting =
    new Position(bbFromAlgSeq(List("b1","g1")),
                 bbFromAlgSeq(List("e1")),
                 bbFromAlgSeq(List("c1","f1")),
                 bbFromAlgSeq(List("a1","h1")),
                 bbFromAlgSeq(List("d1")),
                 bbFromAlgSeq(List("a2","b2","c2","d2","e2","f2","g2","h2")),
                 BitBoard.empty,

                 bbFromAlgSeq(List("b8","g8")),
                 bbFromAlgSeq(List("e8")),
                 bbFromAlgSeq(List("c8","f8")),
                 bbFromAlgSeq(List("a8","h8")),
                 bbFromAlgSeq(List("d8")),
                 bbFromAlgSeq(List("a7","b7","c7","d7","e7","f7","g7","h7")),
                 BitBoard.empty,
                 White,
                 true,
                 true,
                 true,
                 true)
                 
 
  def empty =
    new Position(BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 BitBoard.empty,
                 White,
                 false,
                 false,
                 false,
                 false)

  def possiblePieces =
    for (c <- Seq(Black, White);
         p <- Seq(Pawn(c), Bishop(c), Knight(c), Rook(c), Queen(c), King(c)))
      yield p
}     

class Position(val bbWhiteKnight:Long,
               val bbWhiteKing:Long,
               val bbWhiteBishop:Long,
               val bbWhiteRook:Long,
               val bbWhiteQueen:Long,
               val bbWhitePawn:Long,
               val bbWhiteEnPassant:Long,
               val bbBlackKnight:Long,
               val bbBlackKing:Long,
               val bbBlackBishop:Long,
               val bbBlackRook:Long,
               val bbBlackQueen:Long,
               val bbBlackPawn:Long,
               val bbBlackEnPassant:Long,
               val turn:SideColor,
               val canCastleWhiteKingSide:Boolean, 
               val canCastleWhiteQueenSide:Boolean, 
               val canCastleBlackKingSide:Boolean, 
               val canCastleBlackQueenSide:Boolean) {


  lazy val bbWhite =
    bbWhiteKnight | bbWhiteKing | bbWhiteBishop | bbWhiteRook | bbWhiteQueen | bbWhitePawn

  lazy val bbWhiteCapturable =
    bbWhite | bbWhiteEnPassant

  lazy val bbBlack =
    bbBlackKnight | bbBlackKing | bbBlackBishop | bbBlackRook | bbBlackQueen | bbBlackPawn

  lazy val bbBlackCapturable =
    bbBlack | bbBlackEnPassant


  lazy val bbAll = bbWhite | bbBlack
  lazy val rot90 = bbAll.rotate90
  lazy val rot45 = bbAll.rotate45
  lazy val rot135 = bbAll.rotate135


  def rankAdd(c:SideColor, d:Int) = c match {
    case White => d
    case Black => -d
  }
  
  def bbColor(c:SideColor) = c match {
    case White => bbWhite
    case Black => bbBlack
  }


  def bbCapturable(c:SideColor) = c match {
    case White => bbWhiteCapturable
    case Black => bbBlackCapturable
  }


  def bbClearAll(bb:Long) =
    new Position(bbWhiteKnight & ~bb,
                 bbWhiteKing & ~bb,
                 bbWhiteBishop & ~bb,
                 bbWhiteRook & ~bb,
                 bbWhiteQueen & ~bb,
                 bbWhitePawn & ~bb,
                 bbWhiteEnPassant & ~bb,
                 bbBlackKnight & ~bb,
                 bbBlackKing & ~bb,
                 bbBlackBishop & ~bb,
                 bbBlackRook & ~bb,
                 bbBlackQueen & ~bb,
                 bbBlackPawn & ~bb,
                 bbBlackEnPassant & ~bb,
                 turn,
                 canCastleWhiteKingSide,
                 canCastleWhiteQueenSide,
                 canCastleBlackKingSide,
                 canCastleBlackQueenSide)


  def bbPiece(p:Piece) = p match {
    case Pawn(White) => bbWhitePawn
    case Bishop(White) => bbWhiteBishop
    case Knight(White) => bbWhiteKnight
    case Rook(White) => bbWhiteRook
    case Queen(White) => bbWhiteQueen
    case King(White) => bbWhiteKing
    case Pawn(Black) => bbBlackPawn
    case Bishop(Black) => bbBlackBishop
    case Knight(Black) => bbBlackKnight
    case Rook(Black) => bbBlackRook
    case Queen(Black) => bbBlackQueen
    case King(Black) => bbBlackKing
  }

  def setPiece(p:Piece, v:Long) =
    new Position(
      p match { case Knight(White) => v; case _ => bbWhiteKnight },
      p match { case King(White) => v; case _ => bbWhiteKing },
      p match { case Bishop(White) => v; case _ => bbWhiteBishop },
      p match { case Rook(White) => v; case _ => bbWhiteRook },
      p match { case Queen(White) => v; case _ => bbWhiteQueen },
      p match { case Pawn(White) => v; case _ => bbWhitePawn },
      bbWhiteEnPassant,
      p match { case Knight(Black) => v; case _ => bbBlackKnight },
      p match { case King(Black) => v; case _ => bbBlackKing },
      p match { case Bishop(Black) => v; case _ => bbBlackBishop },
      p match { case Rook(Black) => v; case _ => bbBlackRook },
      p match { case Queen(Black) => v; case _ => bbBlackQueen },
      p match { case Pawn(Black) => v; case _ => bbBlackPawn },
      bbBlackEnPassant,
      turn,
      canCastleWhiteKingSide,
      canCastleWhiteQueenSide,
      canCastleBlackKingSide,
      canCastleBlackQueenSide)

  def setTurn(t:SideColor) =
    new Position(bbWhiteKnight,
                 bbWhiteKing,
                 bbWhiteBishop,
                 bbWhiteRook,
                 bbWhiteQueen,
                 bbWhitePawn,
                 bbWhiteEnPassant,
                 bbBlackKnight,
                 bbBlackKing,
                 bbBlackBishop,
                 bbBlackRook,
                 bbBlackQueen,
                 bbBlackPawn,
                 bbBlackEnPassant,
                 t,
                 canCastleWhiteKingSide,
                 canCastleWhiteQueenSide,
                 canCastleBlackKingSide,
                 canCastleBlackQueenSide)

  def setCastle(side:Piece, v:Boolean) =
    new Position(bbWhiteKnight,
                 bbWhiteKing,
                 bbWhiteBishop,
                 bbWhiteRook,
                 bbWhiteQueen,
                 bbWhitePawn,
                 bbWhiteEnPassant,
                 bbBlackKnight,
                 bbBlackKing,
                 bbBlackBishop,
                 bbBlackRook,
                 bbBlackQueen,
                 bbBlackPawn,
                 bbBlackEnPassant,
                 turn,
                 side match { case King(White) => v; case _ => canCastleWhiteKingSide},
                 side match { case Queen(White) => v; case _ => canCastleWhiteQueenSide},
                 side match { case King(Black) => v; case _ => canCastleBlackKingSide},
                 side match { case Queen(Black) => v; case _ => canCastleBlackQueenSide})

  def clearAllCastle(side:SideColor) = 
    new Position(bbWhiteKnight,
                 bbWhiteKing,
                 bbWhiteBishop,
                 bbWhiteRook,
                 bbWhiteQueen,
                 bbWhitePawn,
                 bbWhiteEnPassant,
                 bbBlackKnight,
                 bbBlackKing,
                 bbBlackBishop,
                 bbBlackRook,
                 bbBlackQueen,
                 bbBlackPawn,
                 bbBlackEnPassant,
                 turn,
                 side match { case White => false; case _ => canCastleWhiteKingSide},
                 side match { case White => false; case _ => canCastleWhiteQueenSide},
                 side match { case Black => false; case _ => canCastleBlackKingSide},
                 side match { case Black => false; case _ => canCastleBlackQueenSide})

  def setEnPassant(side:SideColor, v:Long) =
    new Position(bbWhiteKnight,
                 bbWhiteKing,
                 bbWhiteBishop,
                 bbWhiteRook,
                 bbWhiteQueen,
                 bbWhitePawn,
                 side match { case Black => v; case _ => bbWhiteEnPassant},
                 bbBlackKnight,
                 bbBlackKing,
                 bbBlackBishop,
                 bbBlackRook,
                 bbBlackQueen,
                 bbBlackPawn,
                 side match { case Black => v; case _ => bbBlackEnPassant},
                 turn,
                 canCastleWhiteKingSide,
                 canCastleWhiteQueenSide,
                 canCastleBlackKingSide,
                 canCastleBlackQueenSide)

  def clearAllEnPassant =
    new Position(bbWhiteKnight,
                 bbWhiteKing,
                 bbWhiteBishop,
                 bbWhiteRook,
                 bbWhiteQueen,
                 bbWhitePawn,
                 BitBoard.empty,
                 bbBlackKnight,
                 bbBlackKing,
                 bbBlackBishop,
                 bbBlackRook,
                 bbBlackQueen,
                 bbBlackPawn,
                 BitBoard.empty,
                 turn,
                 canCastleWhiteKingSide,
                 canCastleWhiteQueenSide,
                 canCastleBlackKingSide,
                 canCastleBlackQueenSide)


  def addPiece(p: Piece, pos:Int) = setPiece(p, bbPiece(p).setBit(pos))
  def removePiece(p: Piece, pos:Int) = setPiece(p, bbPiece(p).clearBit(pos))
  def switchPiece(p: Piece, pos:Int) = setPiece(p, bbPiece(p).switchBit(pos))

  def addPiece(p: Piece, pieceList:Seq[Int]) = setPiece(p, bbPiece(p).setListOfBits(pieceList))
  def removePiece(p: Piece, pieceList:Seq[Int]) = setPiece(p, bbPiece(p).clearListOfBits(pieceList))
  def switchPiece(p: Piece, pieceList:Seq[Int]) = setPiece(p, bbPiece(p).switchListOfBits(pieceList))

  def clearAll(pos:Int) = bbClearAll(BitBoard.oneBitBoard(pos))

  def movesForPiece(p:Piece,pos:Int,candidates:Long) = {
    val validDest = candidates & ~(bbColor(p.side))
    validDest.listOfSetBits.map(dest => p match {
      case k:King => KingMove(k, pos, dest)
      case r:Rook => RookMove(r, pos, dest)
      case _ => NormalMove(p, pos, dest)
    })
  }

  def bbToPromotionMoves(side:SideColor,bbPawns:Long) =
    for (s <- bbPawns.listOfSetBits;
         p <- Seq(Bishop(side),Knight(side),Rook(side),Queen(side)))
    yield PawnPromotionMove(Pawn(side),s,p)
  

  def pawnMoves(side:SideColor) = {
    val pawns = bbPiece(Pawn(side))
    val allPieces = bbWhite | bbBlack
    val movableOne = ~(allPieces.shift(0,rankAdd(side,-1)))
    val movableTwo = ~(allPieces.shift(0,rankAdd(side,-2))) & BitBoard.bbPawnOriginalRank(side)
    val bbSingleMove = movableOne & pawns
    val bbSingleMovePromotion = bbSingleMove & BitBoard.bbPawnPromotionRank(side)
    val bbSingleMoveNormal = bbSingleMove ^ bbSingleMovePromotion
    val bbDoubleMove = movableTwo & bbSingleMove
    val singleMovesNormal = bbSingleMoveNormal.listOfSetBits.map(PawnSingleMove(Pawn(side),_))
    val singleMovesPromotion = bbToPromotionMoves(side, bbSingleMovePromotion)
    val doubleMoves = bbDoubleMove.listOfSetBits.map(PawnDoubleMove(Pawn(side),_))
    singleMovesNormal ++ doubleMoves ++ singleMovesPromotion
  }


  def pawnCaptureMoves(side:SideColor) = {
    val pawns = bbPiece(Pawn(side))
    val capturable = bbCapturable(side)
    val leftCapturable = capturable.shift(1,rankAdd(side,-1)) & pawns
    val rightCapturable = capturable.shift(-1,rankAdd(side,-1)) & pawns
    val bbLeftCapturePromotion = leftCapturable & BitBoard.bbPawnPromotionRank(side)
    val bbLeftCaptureNormal = leftCapturable ^ bbLeftCapturePromotion
    val bbRightCapturePromotion = rightCapturable & BitBoard.bbPawnPromotionRank(side)
    val bbRightCaptureNormal = rightCapturable ^ bbRightCapturePromotion
    val leftCapturesNormal =
      bbLeftCaptureNormal.listOfSetBits.map(s => PawnCaptureMove(Pawn(side), s, Square(s.file-1,s.rank+rankAdd(side, 1))))
    val rightCapturesNormal =
      bbRightCaptureNormal.listOfSetBits.map(s => PawnCaptureMove(Pawn(side), s, Square(s.file+1,s.rank+rankAdd(side, 1))))
    val leftCapturesPromotion =
      for (s <- bbLeftCapturePromotion.listOfSetBits;
           p <- (Seq(Bishop(side),Knight(side),Rook(side),Queen(side))))
      yield PawnCapturePromotionMove(Pawn(side), s, Square(s.file-1,s.rank+rankAdd(side, 1)),p)
    val rightCapturesPromotion =
      for (s <- bbLeftCapturePromotion.listOfSetBits;
           p <- (Seq(Bishop(side),Knight(side),Rook(side),Queen(side))))
      yield PawnCapturePromotionMove(Pawn(side), s, Square(s.file+1,s.rank+rankAdd(side, 1)),p)
    leftCapturesNormal ++ rightCapturesNormal ++ leftCapturesPromotion ++ rightCapturesPromotion
  }


  def allMoves = {
    val bishopMoves =
      for (pos <- bbPiece(Bishop(turn)).listOfSetBits;
           candidate <- Seq(BitBoard.diag45Movement(pos, rot45), BitBoard.diag135Movement(pos,rot135));
           m <- movesForPiece(Bishop(turn),pos,candidate))
      yield m

    val queenMoves =
      for (pos <- bbPiece(Queen(turn)).listOfSetBits;
           candidate <- Seq(BitBoard.diag45Movement(pos, rot45),
                            BitBoard.diag135Movement(pos,rot135),
                            BitBoard.rankMovement(pos,rot90),
                            BitBoard.fileMovement(pos,bbAll));
           m <- movesForPiece(Queen(turn),pos,candidate))
      yield m

    val knightMoves =
      for (pos <- bbPiece(Knight(turn)).listOfSetBits;
           m <- movesForPiece(Knight(turn),pos,BitBoard.knightMovement(pos)))
        yield m

    val kingMoves =
      for (pos <- bbPiece(King(turn)).listOfSetBits;
           m <- movesForPiece(King(turn),pos,BitBoard.kingMovement(pos)))
        yield m
    
    val rookMoves =
      for (pos <- bbPiece(Rook(turn)).listOfSetBits;
           candidate <- Seq(BitBoard.rankMovement(pos, rot90), BitBoard.fileMovement(pos,bbAll));
           m <- movesForPiece(Rook(turn),pos,candidate))
      yield m
    
    val allPawnMoves =  pawnMoves(turn) ++ pawnCaptureMoves(turn) 

    allPawnMoves ++ bishopMoves ++ knightMoves ++ rookMoves ++ queenMoves ++ kingMoves
  }

  // slow; checks for legality; not recommended for outside of root node and/or UI
  def makeLegalMove(from:Int, to:Int, promotionPiece:()=>Piece):Option[Position] = {
    val potentials = legalMoves.filter(_.squareFrom == from).filter(_.squareTo == to)
    if (potentials.length == 0)
      None
    else if (potentials.length == 1)
      Some(makeMove(potentials(0)))
    else {
      val winnowed = potentials.filter(_.asInstanceOf[ChessMove {def newPiece:Piece}].newPiece == promotionPiece)
      if (winnowed.length == 0)
        None
      else
        Some(makeMove(winnowed(0)))
    }
  }
      
  def makeLegalMove(move:ChessMove) =
    if (isMoveLegal(move)) Some(makeMove(move)) else None


  // does not check for legality
  def makeMove(move:ChessMove) = {
    val from = move.squareFrom
    val to = move.squareTo
    val p = move.piece
    val newPos = clearAllEnPassant.clearAll(to).removePiece(p, from).addPiece(p, to).setTurn(Opp(p.side))

    move match {
      case m:RookMove => m.side.iterator.foldLeft(newPos)((pos, castleSide) => pos.setCastle(castleSide,false))
      case m:KingMove => newPos.clearAllCastle(p.side)
      case m:PawnDoubleMove =>
        newPos.setEnPassant(p.side, BitBoard.oneBitBoard(from + (p.side match { case Black => -1; case White => 1 })))
      case PawnPromotionMove(_,_,newPiece) =>
        newPos.removePiece(p, to).addPiece(newPiece,to)
      case PawnCapturePromotionMove(_,_,_,newPiece) =>
        newPos.removePiece(p, to).addPiece(newPiece,to)
      case CastleMove(King(Black),true) =>
        newPos.clearAllCastle(p.side).removePiece(Rook(Black),Square("h8")).addPiece(Rook(Black),Square("f8"))
      case CastleMove(King(White),true) =>
        newPos.clearAllCastle(p.side).removePiece(Rook(White),Square("h1")).addPiece(Rook(White),Square("f1"))
      case CastleMove(King(Black),false) =>
        newPos.clearAllCastle(p.side).removePiece(Rook(Black),Square("a8")).addPiece(Rook(Black),Square("d8"))
      case CastleMove(King(White),false) =>
        newPos.clearAllCastle(p.side).removePiece(Rook(White),Square("a1")).addPiece(Rook(White),Square("d1"))
      case _ => newPos
    }
  }

  
  def pieceAt(i: Int):Piece = {
    Position.possiblePieces.foldLeft[Piece](EmptySquare(White))((c, p) => if (bbPiece(p).bit(i)) p else c)
  }


  // only uses pseudo moves - used to check if moves are legal
  def isSquareUnderAttackBy(sq:Int,side:SideColor):Boolean = {
    setTurn(side).allMoves.filter(_.squareTo == sq) == 0
  }

  // is side's king in check?
  def isKingInCheck(side:SideColor):Boolean = {
    val kingLoc = bbPiece(King(side)).findBit.getOrElse(-1)
    isSquareUnderAttackBy(kingLoc, Opp(side))
  }


  // TODO: explore other rules to see what other checks are necessary.  This assumes that the
  // move is at least pseudo-legal
  def isMoveLegal(move: ChessMove) = {
    !makeMove(move).isKingInCheck(turn)
  }

  def legalMoves = allMoves.filter(isMoveLegal(_))


  override def toString = {
    val pieces =
      for (rank <- 7.to(0,-1);
           file <- 0 to 7)
      yield (if (file == 0) "\n " else " ") + (pieceAt(Square(file,rank)) match {
        case Pawn(White) => "wP"
        case Bishop(White) => "wB"
        case Knight(White) => "wN"
        case Rook(White) => "wR"
        case Queen(White) => "wQ"
        case King(White) => "wK"
        case Pawn(Black) => "bP"
        case Bishop(Black) => "bB"
        case Knight(Black) => "bN"
        case Rook(Black) => "bR"
        case Queen(Black) => "bQ"
        case King(Black) => "bK"
        case _ => "  "
      })
    pieces.foldLeft("")(_ + _)
  }
}        

// Chess move in a given position - mainly here because ChessMove class doesn't provide
// enough informatino to convert to PGN notation
case class ChessMovePosition(val position:Position, val move:ChessMove) {
  //TODO: have this use PGN
  override def toString = move.toString
}

object ChessTest extends App {
  Position.starting.makeMove(PawnSingleMove(Pawn(White), Square("e2"))).setTurn(White).allMoves.foreach(println(_))
  println(Position.starting.makeMove(PawnSingleMove(Pawn(White), Square("e2"))).setTurn(White).toString)
  //println(BitBoard.rotatedIndexLookup45.toString)
}
