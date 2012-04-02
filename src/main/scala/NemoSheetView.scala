import scala.swing._
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.ImageIcon
import javax.swing.table.AbstractTableModel

class FormulaRenderer extends DefaultTableCellRenderer {
  override def setValue(v:AnyRef) {
    if (v == null || v.toString == "") {
      setIcon(null)
      setText("")
    }
    else {
      v match {
        case c:NemoCell => c.value match {
          case Some(NemoImage(image)) => {
            val icon = new ImageIcon(image)
            val height = icon.getIconHeight
            val width = icon.getIconWidth
            val t = c.sheetModel.view.get // view has to be available for the renderer to be triggered
            val col = t.peer.getColumnModel.getColumn(c.column)
            col.setPreferredWidth(math.max(col.getPreferredWidth, width))
            //t.setRowHeight(c.row, NemoUtil.newRowHeight(t.peer.getRowHeight(c.row), height))
            t.setRowHeight(c.row, NemoUtil.newRowHeight(height, height))
            setIcon(icon)
          }
          case _ => {
            setIcon(null)
            setText(c.text)
          }
        }
        case _ => {
          setIcon(null);
          setText("Error")
        }
      }
    }
  }
}


case class NemoSheetView(val sheetModel:NemoSheetModel) extends Table {
  var rowHeader:NemoRowHeader = null
  sheetModel.view = Some(this)
  model = sheetModel
  peer.setDefaultRenderer(classOf[AnyRef], new FormulaRenderer)
  selection.elementMode = Table.ElementMode.Cell
  autoResizeMode = Table.AutoResizeMode.Off
  peer.getTableHeader.setReorderingAllowed(false)

  def setRowHeight(row:Int, newHeight:Int) = {
    peer.setRowHeight(row, newHeight)
    rowHeader.peer.setRowHeight(row,newHeight)
  }

  // not entirely sure where this is used, but overriden just in case.  inherited
  // method calls setValueAt, which will flood the undo stack
  override def updateCell(row:Int, col:Int) = sheetModel.cellUpdated(row, col)  
}
