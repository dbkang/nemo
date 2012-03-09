import scala.collection.mutable.Set
import java.awt.image.BufferedImage
import java.awt.Image
import java.awt.Rectangle
import java.awt.Color

// represents each cell in the Nemo table
// TODO: figure out whether TableModel needs to return via getValueAt NemoCell or
// something else like NemoCellValue.  I would think eventually, it's going to be
// NemoCellValue, but not completely certain

class NemoCell(val row:Int, val column:Int) {
  private var formulaValue:String = ""
  private var parsed:Option[Expr] = None
  private var cachedValue:Option[NemoValue] = None

  //def nemotable(col:Int, row:Int):NemoCell

  def formula = formulaValue
  def formula_=(newFormula:String) = {
    // reset parsed expression
    parsed = None
    clearPrecedents

    // recalculate. TODO: this should eventually depend on a global setting
    // TODO: loop detection - loop-causing references should result in parsed
    // set to None
    parsed = NemoParser(newFormula).map(Some(_)).getOrElse(None)
    parsed.foreach(e => findPrecedents(e).foreach(addPrecedent(_)))
    calculate

    // done
    formulaValue = newFormula
  }
  
  val precedents:Set[NemoCell] = Set[NemoCell]()
  val dependents:Set[NemoCell] = Set[NemoCell]()


  // TODO: this method possibly belongs in some other class/object - determine the best place
  def findPrecedents(e:Expr):Seq[NemoCell] = {
    e match {
      case ELit(_) => Seq()
      case ERef(ref) => NemoPreContext.refToNemoCell(ref).toList
      case EAdd(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case ESub(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case EMul(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case EDiv(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case EApply(f, a) => findPrecedents(f) ++ findPrecedents(a)
      case EFun(_,_) => Seq()
      case EList(es) => es.flatMap(findPrecedents _)
      case EIf(c, e1, e2) => findPrecedents(c) ++ findPrecedents(e1) ++ findPrecedents(e2)
      case EEq(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case EAnd(l, r) => findPrecedents(l) ++ findPrecedents(r)
      case EOr(l, r) => findPrecedents(l) ++ findPrecedents(r)
    }
  }  

  // Add a precedent - dependents are managed automatically
  def addPrecedent(p:NemoCell) = {
    p.dependents.add(this)
    precedents.add(p)
  }

  // Clear all precedents
  def clearPrecedents = {
    //clear this cell from its precedents' dependents list first
    precedents.foreach(_.dependents.remove(this))
    precedents.clear
  }

  def text = value.getOrElse("Error").toString
  def address = NemoUtil.columnName(column) + row.toString
  def value = cachedValue //parsed.flatMap(_.eval)
  def calculate:Unit = {
    cachedValue = parsed.flatMap(_.eval(NormalContext(NemoPreContext)))
    //NemoParser.nemoTableReferenced.updateCell(row, column)
    //NemoParser.nemoTableReferenced.model.fireTableCellUpdated(row, column)
    dependents.foreach(_.calculate)
  }
  override def toString = formula
  def toNodeSeq = <cell row={ row.toString } col={ column.toString } formula={ formula } />
}


// represents values stored in each cell
trait NemoValue {
  def valueType:String
  def value:Any
  // up to child classes to override them
  def +(b:NemoValue):NemoValue = NemoError("Not supported")
  def *(b:NemoValue):NemoValue = NemoError("Not supported")
  def /(b:NemoValue):NemoValue = NemoError("Not supported")
  def -(b:NemoValue):NemoValue = NemoError("Not supported")
//  def ==(b:NemoValue):NemoValue = this == b
  override def toString = value.toString
  def toInt:Option[Int] = None
  def toDouble:Option[Double] = toInt.map { _.toDouble }
  def toBoolean:Boolean = true
  def toImage:Option[BufferedImage] = None
}



case class NemoInt(val value:Int) extends NemoValue {
  def valueType = "Int"
  override def +(b:NemoValue) = b.toInt.map(a => NemoInt(a + value)).getOrElse(NemoError("Not supported"))
  override def *(b:NemoValue) = b.toInt.map(a => NemoInt(a * value)).getOrElse(NemoError("Not supported"))
  override def /(b:NemoValue) = b.toInt.map(a => NemoInt(value / a)).getOrElse(NemoError("Not supported"))
  override def -(b:NemoValue) = b.toInt.map(a => NemoInt(value - a)).getOrElse(NemoError("Not supported"))
  override def toInt = Some(value)
  override def toBoolean = (value != 0)
}

case class NemoDouble(val value:Double) extends NemoValue {
  def valueType = "Double"
  override def +(b:NemoValue) = b.toDouble.map(a => NemoDouble(a + value)).getOrElse(NemoError("Not supported"))
  override def *(b:NemoValue) = b.toDouble.map(a => NemoDouble(a * value)).getOrElse(NemoError("Not supported"))
  override def /(b:NemoValue) = b.toDouble.map(a => NemoDouble(value / a)).getOrElse(NemoError("Not supported"))
  override def -(b:NemoValue) = b.toDouble.map(a => NemoDouble(value - a)).getOrElse(NemoError("Not supported"))
  override def toDouble = Some(value.toDouble)
  override def toBoolean = (value != 0.0)
}

case class NemoString(val value:String) extends NemoValue {
  def valueType = "String"
  override def +(b:NemoValue) = NemoString(value + b.toString)
  override def toBoolean = (value != "")
}

case class NemoError(val value:String) extends NemoValue {
  def valueType = "Error"
  override def toString = "Error: " + value
  override def toBoolean = false
}  

object NemoImage {
  def apply(value:BufferedImage, convert:Boolean):NemoImage = {
    if (convert && value.getType != BufferedImage.TYPE_3BYTE_BGR) {
      val image = new BufferedImage(value.getWidth, value.getHeight, BufferedImage.TYPE_3BYTE_BGR)
      val g2d = image.createGraphics
      g2d.drawImage(value,0,0,null)
      g2d.dispose
      apply(image)
    }
    else
      apply(value)
  }
  def apply(width:Int, height:Int):NemoImage = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
    val g2d = image.createGraphics
    g2d.setColor(Color.WHITE)
    g2d.fill(new Rectangle(width, height))
    g2d.dispose
    apply(image)
  }
}      
        

case class NemoImage(val value:BufferedImage) extends NemoValue {
  def valueType = "Image"
  override def toString = "Image"
  override def toImage = Some(value)
  def resize(width:Int, height:Int):NemoImage = {
    val image = NemoImage(width, height)
    val g2d = image.value.createGraphics
    g2d.drawImage(value.getScaledInstance(width,height,Image.SCALE_SMOOTH), 0, 0, null)
    g2d.dispose
    image
  }
}

trait NemoFunction extends NemoValue {
  def apply(args:NemoList):Option[NemoValue]
}  

// TODO: Somehow figure out this context issue.  Right now I'm not sure how to handle
// the fact that the context for the function changes every time you recalculate.  How
// do you automatically cache some results of the calculations and not others (depending
// on the dependency graph) while keeping the context constant?  Do you have to rebuild
// the closure every time?
// TODO: Is there any way to make tail call elimination work?
case class NemoUserFunction(val value:EFun, val context: NemoContext) extends NemoFunction {
  def valueType = "Function"
  override def toString = "Function"
  def apply(args:NemoList) = {
    val c = NormalContext(context)
    var lastVal:Option[NemoValue] = None
    val argsS = args.toSeqOption.get
    c.bindings ++= value.paramList.zip(argsS)
    c.bindings += (("args", NemoList(argsS.takeRight(argsS.length - value.paramList.length))))
    value.body.foreach {s:Statement => lastVal = s.eval(c)}
    lastVal
  }
}

case class NemoPrimitive(val name:String, val value:NemoList=>Option[NemoValue]) extends NemoFunction {
  def valueType = "Primitive"
  override def toString = "#<" + name + ">"
  def apply(args:NemoList) = value(args)
}

// Sort of like a function, but built-in special forms are passed not evaluated arguments
// but raw parsed expressions - this is necessary to implement something like if/cond,
// where some part of the expression should not be evaluated
case class NemoSpecialForm(val value:(NemoContext,EList)=>Option[NemoValue]) extends NemoValue {
  def valueType = "SpecialForm"
  override def toString = "SpecialForm"
}

trait NemoList extends NemoValue {
  def length = 0
  def toSeqOption:Option[Seq[NemoValue]]
  def headOption:Option[NemoValue]
  def tailOption:Option[NemoValue]
  def apply(idx:Int):Option[NemoValue]
}

// blank value
case object NemoUnit extends NemoList {
  def valueType = "Unit"
  def value = ();
  override def toString = "Nil"
  def toSeqOption:Option[Seq[NemoValue]] = Some(Seq[NemoValue]())
  def headOption = None
  def tailOption = None
  def apply(idx:Int) = None
  override def toBoolean = false
}


case class NemoBoolean(val value:Boolean) extends NemoValue {
  def valueType = "Boolean"
  override def toBoolean = value
  override def toInt = if (value) Some(1) else Some(0)
}

object NemoList {
  def nil = NemoUnit
  def cons(h:NemoValue, t:Seq[NemoValue]) = NemoList(h +: t)
  def apply(l:Seq[NemoValue]) = l.foldRight[NemoList](NemoUnit)(NemoCons.apply _)
}

case class NemoCons(var head: NemoValue, var tail:NemoValue) extends NemoList {
  def valueType = "Cons"
  def value = (head.value, tail.value)
  def toSeqOption:Option[Seq[NemoValue]] = {
    tail match {
      case (t:NemoList) => t.toSeqOption.map { head +: _ }
      case _ => None
    }
  }
  def headOption = Some(head)
  def tailOption = Some(tail)
  def apply(idx:Int) = (idx, tail) match {
    case (0, _) => Some(head)
    case (x, y:NemoCons) => y(x - 1)
    case _ => None
  }
}
