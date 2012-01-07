import scala.swing._
import javax.swing.UIManager
import javax.swing.table.DefaultTableCellRenderer

object NemoUtils {
  def columnName(n:Int):String = {
    if (n <= 0)
      ""
    else
      columnName((n - 1) / 26) + ('A' + (n - 1) % 26).toChar.toString
  }
  def colNames(n:Int) = (1 to n).map(columnName(_))

  def colNumber(s:String):Option[Int] = {
    //val chars = s.toUpperCase.toSeq
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
    if (v == null || v.toString == "")
      setText("")
    else
      setText(NemoParser(v.toString).get.eval.get.toString)
  }
}

class NemoTable(val data: Array[Array[Any]], val colnum:Int) extends Table(data, NemoUtils.colNames(colnum)) {
  peer.setDefaultRenderer(classOf[AnyRef], new FormulaRenderer)
  selection.elementMode = Table.ElementMode.Cell
  autoResizeMode = Table.AutoResizeMode.Off
}

class BasicNemo(t:NemoTable) extends ScrollPane(t) {
  def registerReferenceHandler = {
    //def refHandler(ref:String):Option[Int] = {}
    //NemoParser.referenceHandler = (
  }
  rowHeaderView = new NemoRowHeader(t)
  
}

object BasicNemoTest extends SimpleSwingApplication {
//  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  def top = new MainFrame {
    title = "NemoCalc"
    val nemo = new BasicNemo(new NemoTable(Array.ofDim(30,30), 10))
    nemo.registerReferenceHandler
    contents = nemo
  }
}

