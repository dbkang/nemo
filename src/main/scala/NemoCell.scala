import scala.collection.mutable.Set

// represents each cell in the Nemo table
// TODO: figure out whether TableModel needs to return via getValueAt NemoCell or
// something else like NemoCellValue.  I would think eventually, it's going to be
// NemoCellValue, but not completely certain

class NemoCell(val column:Int, val row:Int) {
  private var formulaValue:String = ""
  private var parsed:Option[Expr] = None
  private var cachedValue:Option[Int] = None

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
      case ERef(ref) => NemoParser.refToNemoCell(ref).toList
      case EAddSub(l, rs) => findPrecedents(l) ++ rs.flatMap(r => findPrecedents(r._1))
      case EMulDiv(l, rs) => findPrecedents(l) ++ rs.flatMap(r => findPrecedents(r._1))
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
  def address = NemoUtils.columnName(column) + row.toString
  def value = cachedValue //parsed.flatMap(_.eval)
  def calculate:Unit = {
    cachedValue = parsed.flatMap(_.eval)
    //NemoParser.nemoTableReferenced.updateCell(row, column)
    //NemoParser.nemoTableReferenced.model.fireTableCellUpdated(row, column)
    dependents.foreach(_.calculate)
  }
  override def toString = formula
}
