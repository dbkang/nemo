import swing._

trait Draggable {
//  self: Component {def peer : javax.swing.JComponent{ def getDragEnabled:Boolean }} =>
//  def dragEnabled = peer.getDragEnabled
}

class DraggableListView extends ListView[String] with Draggable {
//  self: Draggable =>
}
