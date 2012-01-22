import scala.swing._
import javax.swing.UIManager
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.AbstractTableModel
import javax.swing.ImageIcon
import java.net.URL
import scala.collection.mutable.Buffer
import scala.collection.mutable.Stack

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

class FormulaRenderer extends DefaultTableCellRenderer {
  override def setValue(v:AnyRef) {
    if (v == null || v.toString == "") {
      setIcon(null)
      setText("")
    }
    else {
      try {
        val v2 = v.asInstanceOf[NemoCell].value.get
        if (v2.valueType == "ImageURL") {
          val c = v.asInstanceOf[NemoCell]
          val icon = new ImageIcon(new URL(v2.asInstanceOf[NemoImageURL].value))
          val height = icon.getIconHeight
          val width = icon.getIconWidth
          val t = NemoParser.nemoTableReferenced
          val col = t.peer.getColumnModel.getColumn(c.column)
          //println("New width: " + width)
          //println("Old width: " + col.getWidth)
          //println("Column number: " + c.column)
          col.setPreferredWidth(math.max(col.getPreferredWidth, width))
          t.setRowHeight(c.row, NemoUtil.newRowHeight(t.peer.getRowHeight(c.row), height))
          setIcon(icon)
        }      
        else {
          setIcon(null)
          setText(v.asInstanceOf[NemoCell].text)
        }
      }
      catch {
        case e => {
          setIcon(null);
          setText("Error")
        }
      }
    }
  }
}


class NemoTable(val rows:Int, val cols:Int) extends Table {
  val columnNames = NemoUtil.colNames(cols)
  val data = Array.ofDim[NemoCell](rows,cols)
  var rowHeader:NemoRowHeader = null
  val undoStack = Stack[(Int,Int,String)]() // row, col, formula - for now
  val redoStack = Stack[(Int,Int,String)]()
  

  def value(row:Int, col:Int) = {
    if (data(row)(col) == null)
      None
    else
      Some(data(row)(col))
  }

  private val _model = new AbstractTableModel {
    override def getColumnName(col: Int) = columnNames(col)
    def getRowCount = data.length
    def getColumnCount = columnNames.length
    def getValueAt(row:Int, col:Int) = value(row, col).getOrElse("")
    override def isCellEditable(row:Int, col:Int) = true
    override def setValueAt(value:Any, row:Int, col:Int) {
      if (data(row)(col) == null) {
        data(row)(col) = new NemoCell(row, col)
      }
      undoStack.push((row, col, data(row)(col).formula))
      redoStack.clear
      data(row)(col).formula = value.toString
      cellUpdated(row, col)
    }

    def cellUpdated(row:Int, col:Int) = {
      fireTableCellUpdated(row, col)
      repaint
    }
  }

  model = _model

  def undo = {
    if (undoStack.length > 0) {
      val (row, col, formula) = undoStack.pop
      redoStack.push((row, col, data(row)(col).formula))
      data(row)(col).formula = formula
      _model.cellUpdated(row, col)
    }
  }

  def redo = {
    if (redoStack.length > 0) {
      val (row, col, formula) = redoStack.pop
      undoStack.push((row, col, data(row)(col).formula))
      data(row)(col).formula = formula
      _model.cellUpdated(row, col)
    }
  }

  peer.setDefaultRenderer(classOf[AnyRef], new FormulaRenderer)
  selection.elementMode = Table.ElementMode.Cell
  autoResizeMode = Table.AutoResizeMode.Off
  peer.getTableHeader.setReorderingAllowed(false)

  def setRowHeight(row:Int, newHeight:Int) = {
    peer.setRowHeight(row, newHeight)
    rowHeader.peer.setRowHeight(row,newHeight)
  }

  override def updateCell(row:Int, col:Int) = {
    model.asInstanceOf[AbstractTableModel].fireTableCellUpdated(row, col)
  }
    
  def apply(ref:String):Option[NemoCell] = {
    NemoUtil.colRowNumbers(ref) match {
      case Some((col, row)) => {
        if (col <= peer.getColumnCount && row <= rowCount) {
          if (data(row-1)(col-1) == null)
            data(row-1)(col-1) = new NemoCell(row-1, col-1)
          value(row-1,col-1)
        }
        else
          None
      }
      case _ => None
    }
  }
}

class BasicNemo(val t:NemoTable) extends ScrollPane(t) {
  NemoParser.nemoTableReferenced = t
  val rh = new NemoRowHeader(t)
  rowHeaderView = rh
  t.rowHeader = rh
}

class NemoContainer(val n:BasicNemo) extends BoxPanel(Orientation.Vertical) {
  contents += new FlowPanel {
    contents += Button("Undo") {
      n.t.undo
    }
    contents += Button("Redo") {
      n.t.redo
    }
  }
  contents += n
}
  

object BasicNemoTest extends SimpleSwingApplication {
//  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  def top = new MainFrame {
    title = "NemoCalc"
    val nemo = new BasicNemo(new NemoTable(512,64))
    contents = new NemoContainer(nemo)
    //contents = nemo
  }
}

