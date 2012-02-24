import scala.collection.mutable.Set

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
      case EApply(f, a) => findPrecedents(a)
      case EFun(_,_) => Seq()
      case EList(es) => es.flatMap(findPrecedents _)
      case EIf(c, e1, e2) => findPrecedents(c) ++ findPrecedents(e1) ++ findPrecedents(e2)
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
  override def toString = value.toString
}


case class NemoInt(val value:Int) extends NemoValue {
  def valueType = "Int"
  def convert(a:NemoValue):Option[NemoInt] = if (a.isInstanceOf[NemoInt]) Some(a.asInstanceOf[NemoInt]) else None    
  override def +(b:NemoValue) = convert(b).map(a => NemoInt(a.value + value)).getOrElse(NemoError("Not supported"))
  override def *(b:NemoValue) = convert(b).map(a => NemoInt(a.value * value)).getOrElse(NemoError("Not supported"))
  override def /(b:NemoValue) = convert(b).map(a => NemoInt(value / a.value)).getOrElse(NemoError("Not supported"))
  override def -(b:NemoValue) = convert(b).map(a => NemoInt(value - a.value)).getOrElse(NemoError("Not supported"))
}

case class NemoDouble(val value:Double) extends NemoValue {
  def valueType = "Double"
  def convert(a:NemoValue):Option[NemoDouble] = {
    if (a.isInstanceOf[NemoDouble])
      Some(a.asInstanceOf[NemoDouble])
    else if (a.isInstanceOf[NemoInt])
      Some(NemoDouble(a.asInstanceOf[NemoInt].value.toDouble))
    else None
  }
  override def +(b:NemoValue) = convert(b).map(a => NemoDouble(a.value + value)).getOrElse(NemoError("Not supported"))
  override def *(b:NemoValue) = convert(b).map(a => NemoDouble(a.value * value)).getOrElse(NemoError("Not supported"))
  override def /(b:NemoValue) = convert(b).map(a => NemoDouble(value / a.value)).getOrElse(NemoError("Not supported"))
  override def -(b:NemoValue) = convert(b).map(a => NemoDouble(value - a.value)).getOrElse(NemoError("Not supported"))

}

case class NemoString(val value:String) extends NemoValue {
  def valueType = "String"
  override def +(b:NemoValue) = NemoString(value + b.toString)
}

case class NemoError(val value:String) extends NemoValue {
  def valueType = "Error"
  override def toString = "Error: " + value
}  

case class NemoImageURL(val value:String) extends NemoValue {
  def valueType = "ImageURL"
  override def toString = "ImageURL: " + value
}

// TODO: Somehow figure out this context issue.  Right now I'm not sure how to handle
// the fact that the context for the function changes every time you recalculate.  How
// do you automatically cache some results of the calculations and not others (depending
// on the dependency graph) while keeping the context constant?  Do you have to rebuild
// the closure every time?
case class NemoFunction(val value:EFun, val context: NemoContext) extends NemoValue {
  def valueType = "Function"
}  
  

// blank value
case object NemoUnit extends NemoValue {
  def valueType = "Unit"
  def value = ();
  override def toString = ""
}

case class NemoList(val value:Seq[NemoValue]) extends NemoValue with Seq[NemoValue] {
  def valueType = "List"
  def apply(idx: Int) = value.apply(idx)
  def length = value.length
  def iterator = value.iterator
}

