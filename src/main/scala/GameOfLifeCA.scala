import scala.collection.mutable.BitSet

trait CellularAutomata[C] {
  def dims:List[Int]
  def rules(neighbors:List[C], currentCell:C): C
}

class BitSet2D(private var width0: Int, private var height0: Int) extends BitSet {
  //  def computeIndex(x:Int, y:Int) = ((y + height) % height) * width + (x + width) % width
  // triangular numbers to map a grid to a line, because this way, the mapping is not dependent
  // upon current dimensions
  def computeIndex(x:Int,y:Int) = {
    val newY = (y + height) % height
    val newX = (x + width) % width
    (0 to (newX + newY)).sum + newX
  }

  def width = width0
  def height = height0

  def apply(x:Int, y:Int):Boolean = apply(computeIndex(x,y))
  def update(x:Int, y:Int, v:Boolean):Unit = update(computeIndex(x,y),v)

  def width_=(w:Int) {
    // zero out previously accessible bits
    if (w < width0)
      for (i <- w until width0;
           j <- 0 until height0)
        this(i,j) = false
    width0 = w
  }

  def height_=(h:Int) {
    // zero out previously accessible bits
    if (h < height0)
      for (i <- 0 until width0;
           j <- h until height0)
        this(i,j) = false
    height0 = h
  }

  def set(x:Int, y:Int):Unit =
    this += computeIndex(x,y)
  def unset(x:Int, y:Int):Unit =
    this -= computeIndex(x,y)
  def set(x:Int, y:Int, v:Boolean):Unit =
    if (v) set(x,y) else unset(x,y)
}

class GameOfLifeCA(private var width0: Int, private var height0: Int, bornIf:List[Int], surviveIf:List[Int])
extends CellularAutomata[Boolean] {
  def width = width0
  def height = height0
  val vals = new BitSet2D(width, height)
  def dims = List(width, height)

  def resize(w:Int,h:Int) {
    vals.width = w
    vals.height = h
    width0 = w
    height0 = h
  }

  def rules(neighbors: List[Boolean], currentCell:Boolean):Boolean = {
    val neighborCount = neighbors.count(_ == true)
    (if (currentCell) surviveIf else bornIf).exists(_ == neighborCount)
  }

  def neighbors(x:Int,y:Int):List[Boolean] =
    for (i <- List.range(-1,2);
         j <- List.range(-1,2) if (i != 0 || j != 0))
    yield vals(x + i, y + j);

  def set(x:Int, y:Int) = vals.set(x,y)
  def unset(x:Int, y:Int) = vals.unset(x,y)
  def set(x:Int, y:Int, v:Boolean) = vals.set(x,y,v)

  def step(callback:((Int,Int,Boolean) => Unit)) = {
    val newVals = new BitSet2D(width, height)
    for (i <- 0 until width;
         j <- 0 until height)
      newVals.set(i, j, rules(neighbors(i, j), vals(i,j)));

    for (i <- 0 until width;
         j <- 0 until height) {
      //      println(if (vals(i,j)) "O" + i + " " + j else "X")
      // neighbors(i, j).map(a => print (if (a) "O" else "X"))
      // println(" " + i + " " + j + " ")
      set(i, j, newVals(i,j))
      callback(i, j, newVals(i,j))
    }
  } 
}


