import scala.swing._
import javax.swing.UIManager
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.AbstractTableModel
import javax.swing.ImageIcon
import java.net.URL
import scala.collection.mutable.Buffer
import scala.collection.mutable.Stack
import scala.xml.NodeSeq
import scala.xml.XML
import scala.xml.Utility
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
          //t.setRowHeight(c.row, NemoUtil.newRowHeight(t.peer.getRowHeight(c.row), height))
          t.setRowHeight(c.row, NemoUtil.newRowHeight(height, height))
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

object NemoTable {
  def saveFile(t:NemoTable, f:File) = {
    XML.save(f.getAbsolutePath, t.toNodeSeq)
  }
  def openFile(f:File):Option[NemoTable] = {
    try {
      val xml = XML.loadFile(f)
      Some(apply(xml))
    } catch {
      case _ => None
    }
  }

  def apply(rows:Int, cols:Int) = {
    val t = new NemoTable(rows, cols)
    NemoParser.nemoTableReferenced = t
    t
  }

  def apply(xml:NodeSeq) = {
    val attribs = xml(0).attributes
    val table = try {
      val rows = attribs("rows")(0).text.toInt
      val cols = attribs("cols")(0).text.toInt
      val t = new NemoTable(rows, cols)
      NemoParser.nemoTableReferenced = t
      xml(0).child.foreach(cell => {
        if (cell.label == "cell") {
          val row = cell.attributes("row")(0).text.toInt
          val col = cell.attributes("col")(0).text.toInt
          t.setFormula(row, col, cell.attributes("formula")(0).text)
        }
      })
      t.repaint
      t
    }
    catch {
      case _ => new NemoTable(512, 64)
    }
    NemoParser.nemoTableReferenced = table
    table
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

  def setFormula(row:Int, col:Int, formula:String) = {
    if (data(row)(col) == null) {
      data(row)(col) = new NemoCell(row, col)
    }
    data(row)(col).formula = formula
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

  def toNodeSeq = {
    <nemotable rows={rows.toString} cols={cols.toString}> {
      for (i <- 0 until rows;
           j <- 0 until cols if !value(i,j).isEmpty)
        yield value(i,j).get.toNodeSeq
    }
    </nemotable>
  }
}

object NemoContainer {
  def apply(t:NemoTable) = {
    val nc = new NemoContainer
    nc.loadNemo(t)
    nc
  }
}

class NemoContainer extends BoxPanel(Orientation.Vertical) {
  class BasicNemo(val t:NemoTable) extends ScrollPane(t) {
    val rh = new NemoRowHeader(t)
    rowHeaderView = rh
    t.rowHeader = rh
  }

  private var bNemo:BasicNemo = null
  def nemo = if (bNemo == null) null else bNemo.t
  def nemoIndex = contents.indexOf(bNemo)
  def loadNemo(t:NemoTable) {
    val i = nemoIndex
    if (i > -1) contents.remove(nemoIndex)
    bNemo = new BasicNemo(t)
    contents += bNemo
    revalidate
    repaint
  }
  contents += new FlowPanel {
    contents += Button("Undo") {
      nemo.undo
    }
    contents += Button("Redo") {
      nemo.redo
    }
    contents += Button("Print") {
      println(nemo.toNodeSeq)
    }
    contents += Button("Load Demo") {
      val demo = <nemotable rows="5" cols="5">
      <cell row="0" col="0" formula="5"/>
      <cell row="1" col="0" formula="10"/>
      <cell row="2" col="0" formula="a1+a2"/>
      </nemotable>
      println(demo)
      loadNemo(NemoTable(demo))
    }
    contents += Button("Open") {
      val d = new FileChooser
      val choice = d.showOpenDialog(null)
      if (choice == FileChooser.Result.Approve)
        NemoTable.openFile(d.selectedFile).foreach(t2 => loadNemo(t2))
    }        

    contents += Button("Save") {
      val d = new FileChooser
      val choice = d.showSaveDialog(null)
      if (choice == FileChooser.Result.Approve)
        NemoTable.saveFile(nemo, d.selectedFile)
    }        
    minimumSize = preferredSize
    maximumSize = preferredSize
  }
}
  

object BasicNemoTest extends SimpleSwingApplication {
  def top = new MainFrame {
    //UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel")
    UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")
    //UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    title = "NemoCalc"
    val nemo = new NemoTable(512,64)
    contents = NemoContainer(nemo)
    //contents = nemo
    centerOnScreen
  }
}

