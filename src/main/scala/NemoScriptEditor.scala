import scala.swing._
import java.awt.Dimension
import scala.swing.event.ListSelectionChanged
import scala.collection.mutable.ArrayBuffer

class NemoScriptEditor(names:Seq[String], scripts:Seq[String]) extends GridBagPanel {
  var selectionIndex = 0
  val saved = scripts.toBuffer
  def buffer(number:Int) = if (selectionIndex == 0) editor.text else saved(number)
  val navigationPanel = new ListView(names){
    preferredSize = new Dimension(300,600)
    selectIndices(selectionIndex)    
  }
  val editor = new TextArea(saved(selectionIndex))
  val editorPane = new ScrollPane(editor) {
    preferredSize = new Dimension(600,450)
  }
  val consolePanel = new Label("Console") {
    preferredSize = new Dimension(600,150)
  }
  val navigationConstraints = new Constraints {
    gridx = 0
    gridy = 0
    gridheight = 2
  }
  val editorConstraints = new Constraints {
    gridx = 1
    gridy = 0
  }
  val consoleConstraints = new Constraints {
    gridx = 1
    gridy = 1
  }
  add(navigationPanel, navigationConstraints)
  add(editorPane, editorConstraints)
  add(consolePanel, consoleConstraints)
  listenTo(navigationPanel.selection)

  reactions += {
    case ListSelectionChanged(_,_,false) => {
      saved(selectionIndex) = editor.text
      selectionIndex = -1 //  until editor.text is set, selectionIndex shouldn't equal document selection
      editor.text = saved(navigationPanel.selection.leadIndex)
      selectionIndex = navigationPanel.selection.leadIndex
    }
  }
}


class ScriptEditorWindow(names:Seq[String], scripts:Seq[String]) extends Frame {
  contents = new ScrollPane(new NemoScriptEditor(names, scripts))
  centerOnScreen
}
