sealed abstract class Expr {
  def eval:Int
}

sealed abstract class Oper

sealed abstract class OpMulDiv extends Oper
sealed abstract class OpAddSub extends Oper

case object OpMul extends OpMulDiv
case object OpDiv extends OpMulDiv
case object OpAdd extends OpAddSub
case object OpSub extends OpAddSub


case class ELit(v:Int) extends Expr {
  def eval = v
}

case class EAddSub(left:Expr, rights:Seq[(Expr,OpAddSub)]) extends Expr {
  def eval = rights.foldLeft(left.eval) {
    (l, r) => r._2 match { case OpAdd => l + r._1.eval; case OpSub => l - r._1.eval }
  }
}

case class EMulDiv(left:Expr, rights:Seq[(Expr,OpMulDiv)]) extends Expr {
  def eval = rights.foldLeft(left.eval) {
    (l, r) => r._2 match { case OpMul => l * r._1.eval; case OpDiv => l / r._1.eval }
  }
}

case class ERef(n:String) extends Expr {
  // TODO: this needs to be wired into the spreadsheet calculation module
  def eval = 5
}

object NemoParser extends scala.util.parsing.combinator.RegexParsers {
  def apply(str:String) = {
    println("Parsing " + str)
    parseAll(expr, str)
  }
  val REF = regex("""[a-zA-z][a-zA-Z]*[1-9][0-9]*"""r) ^^ { ERef(_) }
  val NUM = regex("""[1-9][0-9]*"""r) ^^ { i => ELit(i.toInt) }
  def factor:Parser[Expr] =  "(" ~> expr <~ ")" | REF | NUM 
  def term = factor ~ rep("*" ~> factor ^^ { (_, OpMul) } | "/" ~> factor ^^ { (_, OpDiv) } ) ^^ {
    case l ~ r => EMulDiv(l, r)
  }
  def expr = term ~ rep("+" ~> term ^^ { (_, OpAdd) } | "-" ~> term ^^ { (_, OpSub) } ) ^^ {
    case l ~ r => EAddSub(l, r)
  }
}


//object ParserTest {
//  def apply(str:String) = NemoParser.parseAll(NemoParser.expr, str)
//}
  
