import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

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

object EApply {
  var functions:PartialFunction[String,NemoValue=>Option[NemoValue]] = null
  addPartial { case _ => (a => None) }
  addPartial {
    case "url" => { s => Some(NemoImageURL(s.toString)) }
  }
  addPartial {
    case "command" => { c => {
      import scala.sys.process._
      var stringBuffer:String = ""
      c.toString ! ProcessLogger {
        output => stringBuffer += (output + "\n")
      }
      Some(NemoString(stringBuffer))
    }}
  }
                       

  def addPartial(fun:PartialFunction[String,NemoValue=>Option[NemoValue]]) = {
    if (functions == null)
      functions = fun
    else
      functions = fun.orElse(functions)
  }
}

case class EApply(fun:String, arg:Expr) extends Expr {
  def eval = arg.eval.flatMap(a => EApply.functions(fun)(a))
} 

// Using Parser Combinators to define syntax/parsing rules of Nemo formulas declaratively.
// Each nemo formula is parsed into an instance of Expr.  Any related utility functions also
// belong here
object NemoParser extends StandardTokenParsers {
  var nemoTableReferenced:NemoTable = null
  
  override val lexical = ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")")
  //lexical.reserved ++= List("url")
  val numericLiteral = numericLit ^^ {
    i => if (i.contains(".")) ELit(NemoDouble(i.toDouble)) else ELit(NemoInt(i.toInt))
  }
  val stringLiteral = stringLit ^^ { s => ELit(NemoString(s)) }
  def funCall:Parser[Expr] = ident ~ ("(" ~> expr <~ ")") ^^ { case f ~ e => EApply(f, e) }
  val ref = ident ^^ ERef
  def factor:Parser[Expr] =  "(" ~> expr <~ ")" | numericLiteral | stringLiteral | funCall | ref 
  def term = factor * ("*" ^^^ EMul | "/" ^^^ EDiv)
  def expr = term * ("+" ^^^ EAdd | "-" ^^^ ESub) 

  def refToNemoCell(r:String):Option[NemoCell] = nemoTableReferenced(r)

  def apply(str:String) = {
    println("Parsing " + str)
    phrase(expr)(new lexical.Scanner(str))
  }
}

object ExprLexical extends StdLexical {
  override def token = decimal | super.token
  def decimal = rep(digit) ~ '.' ~ rep1(digit) ^^ {
    case i ~ dot ~ d => NumericLit(i.mkString + "." + d.mkString)
  }
}
