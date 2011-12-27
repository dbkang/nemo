// Methods for manipulating bitboards for the purposes of using them in chess

import scala.collection.immutable.Vector
import Square.fromInt
import BitBoard.fromLong

object BitBoard {
  val empty = 0L
  val full = -1L
  def oneBitBoard(pos:Int) = 1L << pos
  def isBitSet(bb:Long, pos:Int) = (oneBitBoard(pos) & bb) != 0
  implicit def fromLong(v:Long) = new BitBoard(v)
  def apply(v:Long) = fromLong(v)
  def fromSeqOfBits(l:Seq[Int]) = l.foldLeft(empty)((a,b)=> a | oneBitBoard(b))

  private def tri(n: Int) = (0 to n).sum

  private def genRotateIndex(i:Int,deg:Int) = {
    // try this first
    def diagShift(file:Int) = {
      val diag = i.rank + file
      val bubble = if (diag >= 8) diag - 7 else 0
      tri(diag) + file - tri(bubble) - tri(bubble - 1)
    }

    // in case the above is buggy
    def diagShift2(file:Int) = {
      val diag = i.rank + file
      if (diag >= 8)
        63 - tri(15 - diag) + file
      else
        tri(diag) + file
    }

    if (deg == 90)
      Square(i.rank,i.file)
    else if (deg == 45)
      diagShift(i.file)
    else if (deg == 135)
      diagShift(7 - i.file)
    else
      throw new IllegalArgumentException("Only 45, 90 and 135 are valid arguments")
  }

  
  private def from8to12(pos:Int) = (pos.file + 2) * 12 + pos.rank + 2
  private def from12to8(pos:Int) = Square(pos / 12 - 2, pos % 12 - 2)
  private def withinBound(pos:Int) = pos / 12 < 10 && pos / 12 > 1 && pos % 12 < 10 && pos % 12 > 1
  private def kingMoves(pos:Int):List[Int] = {
    val p = from8to12(pos)
    List(p+1,p-1,p+12,p-12,p+13,p-13,p+11,p-11).filter(withinBound _).map(from12to8 _)
  }
  private def knightMoves(pos:Int):List[Int] = {
    val p = from8to12(pos)
    List(p+25,p+23,p-25,p-23,p+14,p+10,p-14,p-10).filter(withinBound _).map(from12to8 _)
  }

  // assuming occ represents piece occupancy list for the given diagonal/file/rank and pos
  // represents the moving piece's location therealong, return the list of positions that
  // are okay to occupy (assume the occupied square is okay to capture; can handle that later)
  def movesFromOcc(pos:Int, occ:Long, max:Int) = ((pos + 1) to max).takeWhile(n => n == pos + 1 || !occ.bit(n - 1)) ++
  (pos-1).to(0,-1).takeWhile(n => n == pos - 1 || !occ.bit(n + 1))
    

  private def fileMoves(pos:Int,occupancy:Int) = {
    empty.setListOfBits(movesFromOcc(pos.rank, occupancy.toLong, 7).map(pos.file * 8 + _))
  }

  private def rankMoves(pos:Int,occupancy:Int) = {
    empty.setListOfBits(movesFromOcc(pos.file, occupancy.toLong, 7).map(_ * 8 + pos.rank))
  }
  
  private def diag45Moves(pos:Int,occupancy:Int) = {
    val diag = pos.file + pos.rank
    val diagLength = 7 - (diag - 7).abs
    val firstInDiag = if (diag < 8) diag else (diag - 7) * 8 + 7
    val currentPosInDiag = (pos - firstInDiag) / 7
    fromSeqOfBits(movesFromOcc(currentPosInDiag, occupancy.toLong, diagLength).map(_ * 7 + firstInDiag))
  }    


  private def diag135Moves(pos:Int,occupancy:Int) = {
    val diag = pos.rank + 7 - pos.file
    val diagLength = 7 - (diag - 7).abs
    val firstInDiag = if (diag < 8) 7 * 8 + diag else (14 - diag) * 8 + 7
    val currentPosInDiag = (pos - firstInDiag) / (-9)
    fromSeqOfBits(movesFromOcc(currentPosInDiag, occupancy.toLong, diagLength).map(_ * (-9) + firstInDiag))
  }    

  lazy val rotatedIndexLookup45 = Vector.range(0,64).map(genRotateIndex(_, 45))
  lazy val rotatedIndexLookup90 = Vector.range(0,64).map(genRotateIndex(_, 90))
  lazy val rotatedIndexLookup135 = Vector.range(0,64).map(genRotateIndex(_, 135))

  lazy val kingMovement = 
    Vector.range(0,64).map(a => empty.setListOfBits(kingMoves(a)))
  lazy val knightMovement =
    Vector.range(0,64).map(a => empty.setListOfBits(knightMoves(a)))
  lazy val rankMovementVector =
    Vector.range(0,64).map(a => Vector.range(0,256).map(b => rankMoves(a, b)))
  lazy val fileMovementVector =
    Vector.range(0,64).map(a => Vector.range(0,256).map(b => fileMoves(a, b)))
  lazy val diag45MovementVector =
    Vector.range(0,64).map(a => Vector.range(0,256).map(b => diag45Moves(a, b)))
  lazy val diag135MovementVector = 
    Vector.range(0,64).map(a => Vector.range(0,256).map(b => diag135Moves(a, b)))


  def diag45Movement(pos:Int,rot45:Long) = {
    val diag = pos.rank + pos.file
    val shift = if (diag < 8) rotatedIndexLookup45(diag) else rotatedIndexLookup45(Square(diag-7,7))
    val diagLength = 7 - (7 - diag).abs
    val offset = oneBitBoard(diagLength + 1) - 1
    val occ = ((rot45 >>> shift) & offset).toInt
    diag45MovementVector(pos)(occ)
  }

  def diag135Movement(pos:Int,rot135:Long) = {
    val diag = pos.rank + 7 - pos.file
    // looks wrong, but I think it's correct.  Easier to use Rot45 to calc offsets
    val shift = if (diag < 8) rotatedIndexLookup45(diag) else rotatedIndexLookup45(Square(diag-7,7))
    val diagLength = 7 - (7 - diag).abs
    val offset = oneBitBoard(diagLength + 1) - 1
    val occ = ((rot135 >>> shift) & offset).toInt
    diag135MovementVector(pos)(occ)
  }

  def rankMovement(pos:Int, rot90:Long) = {
    //rot90.listOfSetBits.foreach(println(_))
    //rankMovementVector(pos)(((rot90 >>> (pos.rank * 8)) & 255L).toInt).listOfSetBits.foreach(println(_))
    rankMovementVector(pos)(((rot90 >>> (pos.rank * 8)) & 255L).toInt)
  }
  def fileMovement(pos:Int, occ:Long) = fileMovementVector(pos)(((occ >>> (pos.file * 8)) & 255L).toInt)    

  val rank7 = fromSeqOfBits(Square(Seq("a7","b7","c7","d7","e7","f7","g7","h7")))
  val rank2 = fromSeqOfBits(Square(Seq("a2","b2","c2","d2","e2","f2","g2","h2")))
  def bbPawnOriginalRank(side:SideColor) = side match { case Black => rank7; case White => rank2 }
  def bbPawnPromotionRank(side:SideColor) = side match { case Black => rank2; case White => rank7 }

}  

class BitBoard(val self:Long) {
  // if non-zere return Some(firstbitset), otherwise None. TODO: this could both be optimized
  // and lazy-val'ed
  def findBit:Option[Int] = (0 to 63).find(bit(_))
  def setBit(pos:Int):Long = self | BitBoard.oneBitBoard(pos)
  def setBit(pos:Int, v:Boolean):Long = if (v) setBit(pos) else self
  def bit(pos:Int) = (self & BitBoard.oneBitBoard(pos)) != 0
  def switchBit(pos:Int) = self ^ BitBoard.oneBitBoard(pos)
  def clearBit(pos:Int) = self & BitBoard.full.switchBit(pos)
  def switchListOfBits(l:Seq[Int]) = self ^ BitBoard.fromSeqOfBits(l)
  def setListOfBits(l:Seq[Int]) = self | BitBoard.fromSeqOfBits(l)
  def clearListOfBits(l:Seq[Int]) = self & ~(BitBoard.fromSeqOfBits(l))
  def listOfSetBits = (0 to 63).filter(bit(_))
  def count = listOfSetBits.length
  def shift(fileD:Int, rankD:Int) = {
    val shiftAmount = Square(fileD, rankD)
    if (shiftAmount < 0)
      self >>> -shiftAmount
    else if (shiftAmount > 0)
      self << shiftAmount
    else
      self
  }
  def foldLeft[A](z: A)(op:(A,Int,Boolean) => A):A =
    (0 to 63).foldLeft(z)((bb, i) => op(bb, i, bit(i)))
  def rotate90 =
    foldLeft(BitBoard.empty)((bb, i, v) => bb.setBit(BitBoard.rotatedIndexLookup90(i), v))
  def rotate45 =
    foldLeft(BitBoard.empty)((bb, i, v) => bb.setBit(BitBoard.rotatedIndexLookup45(i), v))
  def rotate135 =
    foldLeft(BitBoard.empty)((bb, i, v) => bb.setBit(BitBoard.rotatedIndexLookup135(i), v))

}

object BitBoardTest extends App {
  for (i <- 0 to 63) {
    println("" + i + " " + BitBoard.rotatedIndexLookup135(i))
  }
}
