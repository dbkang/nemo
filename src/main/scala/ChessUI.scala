import swing._
import Square.fromInt
import scala.swing.event.ActionEvent
import javax.swing.ImageIcon
import javax.swing.UIManager


trait ChessIconSet {
  def apply(p:Piece,squareColor:SideColor):String
}

trait ChessClock {
  def remainingTime(sc:SideColor):Int
}

trait ChessPlayer {
  def position:Position
  def nextMove:ChessMove
  def makeMove(m:ChessMove):Unit
}

trait ChessGame {
  def position:Position
  def position_=(p:Position):Unit
  def clock:ChessClock
  def player(side:SideColor):ChessPlayer
  def takeback:Unit
  def history:Seq[ChessMovePosition]
}


class SquareCell(val loc: Int) extends ToggleButton {
  icon = new ImageIcon("/usr/share/icons/hicolor/48x48/apps/kdiamond.png","")
  selectedIcon = new ImageIcon("/home/eitay/youngbb.jpg","")
  margin = new Insets(0,0,0,0)
  def color = if ((loc.file + loc.rank) % 2 == 0) Black else White
  private var _piece:Piece = EmptySquare(color)
  def piece = _piece
  def piece_= (p:Piece):Unit = {
    _piece = p
    // TODO: then change the icon accordingly
  }
}


class ChessBoardPanel extends GridPanel(8,8) {
  // val selected = scala.collection.mutable.IndexedSeq.fill(() => false)
  private var _selectedCell:Option[Int] = None
  def selectedCell = _selectedCell
  def selectedCell_=(s:Option[Int]):Unit = {
    println(board.toString)
    board.legalMoves.foreach(println(_))
    _selectedCell match {
      case Some(ps) => cell(ps).selected = false
      case None =>
    }
    _selectedCell = s
    println(_selectedCell.toString)
  }

  def setupCells = {
    for (i <- 0 until 8;
         j <- 0 until 8) {
      val sc = new SquareCell(Square(j, 7 - i))
      contents += sc
      listenTo(sc)
    }
  }

  def cell(s:Int):SquareCell = contents(Square(7 - s.rank, s.file)).asInstanceOf[SquareCell]
  def cell(file:Int,rank:Int):SquareCell = cell(Square(file,rank))

  var _board = Position.starting
  def board = _board
  def board_=(b:Position):Unit = {
    _board = b
    // TODO: also update the cells
  }

  // TODO: design a dialog box that asks for which piece
  def askForPromotion = {
    Queen(board.turn)
  }

  // returns true if move is made
  def makeMove(from:Int, to:Int) = {
    val newB = board.makeLegalMove(from, to, askForPromotion _)
    newB match {
      case Some(b) => { board = b; true }
      case None => false
    }
  }

  reactions += {
    case ActionEvent(sc: SquareCell) => {
      println("Something got pushed")
      selectedCell match {
        case None => selectedCell = Some(sc.loc)
        case Some(preLoc) => {
          selectedCell = Some(sc.loc)
          if (makeMove(preLoc, sc.loc))
            selectedCell = None
        }
      }
    }
  }
  setupCells
}

class ChessGamePanel(val chessboard:ChessBoardPanel) extends BoxPanel(Orientation.Vertical) {
  contents += new FlowPanel {
    contents += new Button("Some silly button")
    contents += new Button("Another silly button")
  }
  contents += new ScrollPane(chessboard)
}

class ConsolePanel(val c:Component) extends BoxPanel(Orientation.Horizontal) {

  val textArea = new TextArea {
    font = new java.awt.Font("Monospaced.plain", java.awt.Font.PLAIN, 12)
    columns = 35
    editable = false
  }

  object TextAreaOutputStream extends java.io.OutputStream {
    override def write(b:Array[Byte],off:Int,len:Int) = {
      textArea.append(new String(b,off,len))
    }
    override def write(b:Array[Byte]) = {
      textArea.append(new String(b))
    }
    override def write(b:Int) = {
      write(Array(b.asInstanceOf[Byte]))
    }
  }
  contents += c
  contents += new ScrollPane(textArea)
  Console.setOut(TextAreaOutputStream)

}

object ChessUI extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
  def top = new MainFrame {
    title = "Chess"
    contents = new ConsolePanel(new ChessGamePanel(new ChessBoardPanel))
  }
}
