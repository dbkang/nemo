import scala.swing.Table
import javax.swing.table.AbstractTableModel
import scala.collection.mutable.Buffer
import scala.collection.mutable.Stack
import scala.xml.NodeSeq
import scala.xml.XML
import scala.xml.Utility
import scala.collection.mutable.StringBuilder
import java.io.File

object NemoUtil {
  var defaultRowHeight = 16
  
  def newRowHeight(current:Int, newMinimumRequired:Int) =
    math.max(math.max(current, newMinimumRequired), defaultRowHeight)


  def columnName(n:Int):String = {
    if (n <= 0)
      ""
    else
      columnName((n - 1) / 26) + ('A' + (n - 1) % 26).toChar.toString
  }

  def colNames(n:Int) = (1 to n).map(columnName(_))

  def colNumber(s:String):Option[Int] = {
    val colN = s.toUpperCase.foldLeft(0)((result, c) => result * 26 + c - 'A' + 1)
    if (colN > 0) Some(colN) else None
  }

  def rowNumber(s:String):Option[Int] = {
    try {
      Some(s.toInt)
    }
    catch {
      case e => None
    }
  }

  def colRowNumbers(s:String):Option[(Int,Int)] = {
    val left = s.toUpperCase.takeWhile(a => a <= 'Z' && a >= 'A')
    val right = s.takeRight(s.length - left.length)
    (colNumber(left), rowNumber(right)) match {
      case (Some(col), Some(row)) => Some((col,row))
      case _ => None
    }
  }
}


object NemoSheetModel {
  def saveFile(t:NemoSheetModel, f:File) = {
    XML.save(f.getAbsolutePath, t.toNodeSeq)
  }

  def openFile(f:File):Option[NemoSheetModel] = {
    try {
      val xml = XML.loadFile(f)
      Some(apply(xml))
    } catch {
      case _ => None
    }
  }

  def apply(rows:Int, cols:Int) = new NemoSheetModel(rows, cols)

  def apply(xml:NodeSeq) = {
    val attribs = xml(0).attributes
    try {
      val rows = attribs("rows")(0).text.toInt
      val cols = attribs("cols")(0).text.toInt
      new NemoSheetModel(rows, cols) {
        xml(0).child.foreach(cell => {
          if (cell.label == "cell") {
            val row = cell.attributes("row")(0).text.toInt
            val col = cell.attributes("col")(0).text.toInt
            setFormula(row, col, cell.attributes("formula")(0).text)
          }
        })
      }
    }
    catch {
      case _ => new NemoSheetModel(512, 64)
    }
  }
}

class NemoSheetModel(rows:Int, cols:Int) extends AbstractTableModel {
  val data = Array.ofDim[NemoCell](rows,cols)
  val undoStack = Stack[(Int,Int,String)]() // row, col, formula - for now
  val redoStack = Stack[(Int,Int,String)]()
  val columnNames = NemoUtil.colNames(cols)
  val context = NemoSheetContext(NemoPreContext, this)
  var view:Option[NemoSheetView] = None

  // does the same thing as setValueAt, but without affecting undo/redo stack.
  // used for file load
  def setFormula(row:Int, col:Int, formula:String) = {
    if (data(row)(col) == null) {
      data(row)(col) = new NemoCell(row, col, this)
    }
    data(row)(col).formula = formula
  }

  def value(row:Int, col:Int) = {
    if (data(row)(col) == null)
      None
    else
      Some(data(row)(col))
  }

  def undo = {
    if (undoStack.length > 0) {
      val (row, col, formula) = undoStack.pop
      redoStack.push((row, col, data(row)(col).formula))
      data(row)(col).formula = formula
      cellUpdated(row, col)
    }
  }

  def redo = {
    if (redoStack.length > 0) {
      val (row, col, formula) = redoStack.pop
      undoStack.push((row, col, data(row)(col).formula))
      data(row)(col).formula = formula
      cellUpdated(row, col)
    }
  }

  def apply(ref:String):Option[NemoCell] = {
    NemoUtil.colRowNumbers(ref) match {
      case Some((col, row)) => {
        if (col <= getColumnCount && row <= getRowCount) {
          if (data(row-1)(col-1) == null)
            data(row-1)(col-1) = new NemoCell(row-1, col-1, this)
          value(row-1,col-1)
        }
        else
          None
      }
      case _ => None
    }
  }

  def toNodeSeq = {
    <nemotable rows={rows.toString} cols={cols.toString}> {
      for (i <- 0 until rows;
           j <- 0 until cols if !value(i,j).isEmpty)
        yield value(i,j).get.toNodeSeq
    }
    </nemotable>
  }

  override def getColumnName(col: Int) = columnNames(col)
  def getRowCount = data.length
  def getColumnCount = columnNames.length
  def getValueAt(row:Int, col:Int) = value(row, col).getOrElse("")
  override def isCellEditable(row:Int, col:Int) = true
  override def setValueAt(value:Any, row:Int, col:Int) {
    if (data(row)(col) == null) {
      data(row)(col) = new NemoCell(row, col, this)
    }
    undoStack.push((row, col, data(row)(col).formula))
    redoStack.clear
    data(row)(col).formula = value.toString
    cellUpdated(row, col)
  }
  
  def cellUpdated(row:Int, col:Int) = {
    fireTableCellUpdated(row, col)
    view.map { _.repaint }
  }
}

