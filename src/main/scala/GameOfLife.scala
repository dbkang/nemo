import swing._
//import javax.swing._
import javax.swing.ImageIcon
import java.awt.Dimension
import java.awt.Insets
import scala.swing.event.ActionEvent
import scala.swing.event.ValueChanged
import javax.swing.UIManager

class GridCell(val cellRow:Int, val cellCol:Int) extends ToggleButton {
  icon = new ImageIcon("/usr/share/icons/hicolor/48x48/apps/kdiamond.png","")
  selectedIcon = new ImageIcon("/home/eitay/youngbb.jpg","")
//  selectedIcon = new ImageIcon("/usr/share/icons/hicolor/48x48/apps/gimp.png","")
  margin = new Insets(0,0,0,0)
}


class CellGridPanel(val numRows:Int, val numCols:Int) extends GridPanel(numRows, numCols) {
  
  def setupCells = {
    for (i <- 0 until columns;
         j <- 0 until rows) {
      val gc = new GridCell(i,j)
      contents += gc
      listenTo(gc)
    }
  }

  def cell(x:Int,y:Int) = contents(y * columns + x)

  val gameCA = new GameOfLifeCA(numCols, numRows, List(3),List(2,3))

  def setCellDisplay(x:Int, y:Int, v:Boolean) = 
    cell(x,y).asInstanceOf[GridCell].selected = v

  def setCellDisplayAndCA(x:Int, y:Int, v:Boolean) = {
    setCellDisplay(x,y,v)
    gameCA.set(x,y,v)
  }

  def getCellDisplay(x:Int, y:Int):Boolean = cell(x,y).asInstanceOf[GridCell].selected

  def step = gameCA.step(setCellDisplay _)
  
  def resize(newNumRows:Int,newNumCols:Int) = {
    contents.clear
    rows = newNumRows
    columns = newNumCols
    gameCA.resize(newNumCols,newNumRows)
    setupCells
    revalidate
    repaint
  }
    

  reactions += {
    case ActionEvent(gc:GridCell) => {
      gameCA.set(gc.cellCol,gc.cellRow,gc.selected)
    }
  }
  setupCells
}

case class TimerEvent() extends scala.swing.event.Event  

class Timer(val delay0:Int) extends javax.swing.Timer(delay0, null) with Publisher {
  def this(delay0:Int, action:(()=>Unit)) = {
    this(delay0)
    reactions += {
      case TimerEvent() => action()
    }
  }

  addActionListener(new java.awt.event.ActionListener {
    def actionPerformed(e: java.awt.event.ActionEvent) = publish(TimerEvent())
  })
}


class GameOfLifePanel(val gameGrid:CellGridPanel) extends BoxPanel(Orientation.Vertical) {
  val timer = new Timer(500, gameGrid.step _)

  class StartStopButton extends Button("Start") {
    reactions += {
      case ActionEvent(a:StartStopButton) =>
        if (a.text == "Start") {
          a.text = "Stop"
          timer.start
        }
        else {
          a.text = "Start"
          timer.stop
        }
    }
  }

  contents += new FlowPanel {
    class ButtonLinkedField(val id:String) extends FormattedTextField(java.text.NumberFormat.getIntegerInstance()) {
      peer.setColumns(3)
      def value = peer.getValue.asInstanceOf[Number].intValue
    }
    val rowField = new ButtonLinkedField("row")
    val colField = new ButtonLinkedField("col")

    contents += new StartStopButton

    contents += (Button("Step") {
      gameGrid.step
    })

    contents += (Button("Change Icon") {
      (new FileChooser).showOpenDialog(this)
    })

    contents += new Label("Rows:")
    contents += rowField
    contents += new Label("Columns:")
    contents += colField

    contents += new Button("Resize") {
      enabled = false
      listenTo(rowField)
      listenTo(colField)
      reactions += {
        case ActionEvent(_) =>
          gameGrid.resize(rowField.value, colField.value)
          enabled = false
        case ValueChanged(_) =>
          enabled = true
      }
    }
  }
  contents += new ScrollPane(gameGrid) 
}


object GameOfLife extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  //UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel")
  def top = new MainFrame {
    title = "Game of Life"
    contents = new GameOfLifePanel(new CellGridPanel(15,15))
  }

}
