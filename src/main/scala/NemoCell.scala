import scala.collection.mutable.Set

// represents each cell in the Nemo table
// TODO: figure out whether TableModel needs to return via getValueAt NemoCell or
// something else like NemoCellValue.  I would think eventually, it's going to be
// NemoCellValue, but not completely certain

class NemoCell(val row:Int, val column:Int, val sheetModel:NemoSheetModel) {
  private var formulaValue:String = ""
  private var parsed:Option[Expr] = None
  private var cachedValue:Option[NemoValue] = Some(NemoUnit)

  //def nemotable(col:Int, row:Int):NemoCell

  def formula = formulaValue
  def formula_=(newFormula:String) = {
    // reset parsed expression
    parsed = None
    clearPrecedents

    // recalculate. TODO: this should eventually depend on a global setting
    // TODO: loop detection - loop-causing references should result in parsed
    // set to None
    parsed = (newFormula, NemoParser(newFormula)) match {
      case ("", _) => Some(ELit(NemoUnit))
      case (_, NemoParser.Success(e, _)) => Some(e)
      case (_, NemoParser.Failure(msg, _)) => Some(ELit(NemoError(msg)))
      case (_, NemoParser.Error(msg, _)) => Some(ELit(NemoError(msg)))
      case _ => Some(ELit(NemoError("Unknown Error")))
    }

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
      case ERef(ref) => sheetModel(ref).toList
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
    cachedValue = parsed.flatMap(_.eval(NormalContext(sheetModel.context)))
    //NemoParser.nemoTableReferenced.updateCell(row, column)
    //NemoParser.nemoTableReferenced.model.fireTableCellUpdated(row, column)
    dependents.foreach(_.calculate)
  }
  override def toString = formula
  def toNodeSeq = <cell row={ row.toString } col={ column.toString }>{ formula }</cell>
}

