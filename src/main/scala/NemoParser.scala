//import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed abstract class Expr {
  def eval:Option[NemoValue]
}

case class ELit(v:NemoValue) extends Expr {
  def eval = Some(v)
}

case class EAdd(l:Expr, r:Expr) extends Expr {
  def eval = for (i <- l.eval; j <- r.eval) yield i + j
}

case class ESub(l:Expr, r:Expr) extends Expr {
  def eval = for (i <- l.eval; j <- r.eval) yield i - j
}

case class EMul(l:Expr, r:Expr) extends Expr {
  def eval = for (i <- l.eval; j <- r.eval) yield i * j
}

case class EDiv(l:Expr, r:Expr) extends Expr {
  def eval = for (i <- l.eval; j <- r.eval) yield i / j
}

case class ERef(r:String) extends Expr {
  def eval = NemoParser.refToNemoCell(r).flatMap(_.value)
}


// Using Parser Combinators to define syntax/parsing rules of Nemo formulas declaratively.
// Each nemo formula is parsed into an instance of Expr.  Any related utility functions also
// belong here
object NemoParser extends StandardTokenParsers {
  var nemoTableReferenced:NemoTable = null

  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")
  val numericLiteral = numericLit ^^ { i => ELit(NemoInt(i.toInt)) }
  val ref = ident ^^ ERef
  def factor:Parser[Expr] =  "(" ~> expr <~ ")" | numericLiteral | ref

  def term = factor * ("*" ^^^ EMul | "/" ^^^ EDiv)
  def expr = term * ("+" ^^^ EAdd | "-" ^^^ ESub)

  def refToNemoCell(r:String):Option[NemoCell] = nemoTableReferenced(r)

  def apply(str:String) = {
    println("Parsing " + str)
    phrase(expr)(new lexical.Scanner(str))
  }
}
