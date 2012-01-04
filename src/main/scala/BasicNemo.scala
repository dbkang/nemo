import scala.swing._
import javax.swing.UIManager


object NemoUtils {
  def columnName(n:Int):String = {
    if (n <= 0)
      ""
    else
      columnName((n - 1) / 26) + ('A' + (n - 1) % 26).toChar.toString
  }
  def colNames(n:Int) = (1 to n).map(columnName(_))
//  def blankData(rowNum:Int, colNum:Int) = Array(

}

class FormulaRenderer extends javax.swing.table.DefaultTableCellRenderer {
  override def setValue(v:AnyRef) {
    if (v == null || v.toString == "")
      setText("")
    else
      setText(NemoParser(v.toString).get.eval.toString)
  }
}
class BasicNemo(val data: Array[Array[Any]], val colnum:Int) extends Table(data, NemoUtils.colNames(colnum)) {
  peer.setDefaultRenderer(classOf[AnyRef], new FormulaRenderer)
}

object BasicNemoTest extends SimpleSwingApplication {
//  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  def top = new MainFrame {
    title = "NemoPad"
    contents = new ConsolePanel(new ScrollPane(new BasicNemo(Array.ofDim(20,10), 10)))
  }
}

