import javax.swing.table.AbstractTableModel
import javax.swing.table.DefaultTableCellRenderer
import java.awt.Dimension
import swing.Table
import javax.swing.JTable
import javax.swing.UIManager

class NemoRowHeader(t:Table) extends Table {
  //override def getDefaultRenderer(c:Class[_]) = RowHeaderRenderer
  //override def getPreferredScrollableViewportSize = new Dimension(32, peer.getPreferredSize.height)
  object RowHeaderModel extends AbstractTableModel {
    def getRowCount = t.model.getRowCount
    def getColumnCount = 1
    def getValueAt(row:Int, col:Int) = (row + 1).toString
  }

  object RowHeaderRenderer extends DefaultTableCellRenderer {
    override def getTableCellRendererComponent(t:JTable, v:Any, selected:Boolean,
                                               hasFocus:Boolean, row:Int, col:Int) = {
      setBackground(UIManager.getColor("TableHeader.background"))
      setForeground(UIManager.getColor("TableHeader.foreground"))
      setBorder(UIManager.getBorder("TableHeader.cellBorder"))
      setFont(UIManager.getFont("TableHeader.font"))
      setHorizontalAlignment(0) //setHorizontalAlignment(CENTER)
      setValue(v)
      this
    }
  }


  peer.setDefaultRenderer(classOf[AnyRef], RowHeaderRenderer)
  preferredViewportSize = new Dimension(32, peer.getPreferredSize.height)
  model = RowHeaderModel
  rowHeight = t.rowHeight
  peer.setIntercellSpacing(new Dimension(0,0))
  peer.setShowHorizontalLines(false)
  peer.setShowVerticalLines(false)
  
}
                    
     
