import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.collection.mutable.Map

sealed abstract class Expr {
  def eval(c: NemoContext):Option[NemoValue]
}

sealed abstract class Statement {
  def eval(c: NemoContext):Option[NemoValue]
}

case class SExpr(e: Expr) extends Statement {
  def eval(c: NemoContext) = e.eval(c)
}

case class SLet(b:String, e: Expr) extends Statement {
  def eval(c: NemoContext):Option[NemoValue] = {
    val ev = e.eval(c)
    ev.foreach(v => c.add(b, v))
    ev
  }
}

case class EIf(cond:Expr, e1:Expr, e2:Expr) extends Expr {
  def eval(c:NemoContext):Option[NemoValue] = {
    val x = cond.eval(c)
    if (x match {
      case Some(NemoInt(v)) => v != 0
      case Some(NemoDouble(v)) => v != 0.0
      case Some(NemoString(v)) => v != ""
      case Some(NemoUnit) => false
      case None => false
      case Some(NemoError(_)) => false
      case Some(NemoBoolean(v)) => v
      case _ => true
    }) e1.eval(c) else e2.eval(c)
  }
}

case class ELit(v:NemoValue) extends Expr {
  def eval(c: NemoContext) = Some(v)
}

case class EAdd(l:Expr, r:Expr) extends Expr {
  def eval(c: NemoContext) = for (i <- l.eval(c); j <- r.eval(c)) yield i + j
}

case class ESub(l:Expr, r:Expr) extends Expr {
  def eval(c: NemoContext) = for (i <- l.eval(c); j <- r.eval(c)) yield i - j
}

case class EMul(l:Expr, r:Expr) extends Expr {
  def eval(c: NemoContext) = for (i <- l.eval(c); j <- r.eval(c)) yield i * j
}

case class EDiv(l:Expr, r:Expr) extends Expr {
  def eval(c: NemoContext) = for (i <- l.eval(c); j <- r.eval(c)) yield i / j
}

case class EEq(l:Expr, r:Expr) extends Expr {
  def eval(c: NemoContext) = for (i <- l.eval(c); j <- r.eval(c)) yield NemoBoolean(i == j)
}

case class ERef(r:String) extends Expr {
  def eval(c: NemoContext) = c(r)
}

//case class EBody(Seq[Statement]) extends Expr {
//  def eval(c: NemoContext) = {
    

case class EFun(val paramList:Seq[String], val body:Seq[Statement]) extends Expr {
  def eval(c: NemoContext) = Some(NemoUserFunction(this, c))
}

object EList {
  //def cons(head:Expr, tail:EList) = EList(head +: tail.es)
  def append(l:EList, t:Expr) = EList(l.es :+ t)
}

case class EList(val es: Seq[Expr]) extends Expr {
  def eval(c: NemoContext):Option[NemoList] = {
    val result = 
      for (e <- es;
           v <- e.eval(c)) yield v
    if (result.length == es.length) Some(NemoList(result)) else None
  }
  def length = es.length
}

case class EApply(fun:Expr, args:EList) extends Expr {
  def eval(c:NemoContext) = {
    val cf = fun.eval(c)
    cf match {
      case None => Some(NemoError("Function not found"))
      case Some(NemoSpecialForm(f)) => f(c, args)      
      case Some(f:NemoFunction) => args.eval(c).flatMap(f.apply _)
      case _ => Some(NemoError("Not a function"))
    }
  }
}
  
abstract class NemoContext {
  val bindings = Map[String, NemoValue]()
  def apply(name:String):Option[NemoValue]
  def add(name: String, value:NemoValue) = {
    bindings += ((name, value))
  }
}

// this is the base context/scope that simply defines cell references
case object NemoPreContext extends NemoContext {
  def addPrimitive(name:String, function:NemoList=>Option[NemoValue]) = {
    bindings += ((name, NemoPrimitive(name, function)))
  }

  bindings += (("nil", NemoList.nil))

  addPrimitive("url", args => {
    args.headOption.map(v => NemoImageURL(v.toString))
  })
               
  addPrimitive("command", args => {
    import scala.sys.process._
    var stringBuffer:String = ""
    args.headOption.map {
      v:NemoValue => v.toString ! ProcessLogger {
        output => stringBuffer += (output + "\n")
      }
      NemoString(stringBuffer)
    }
  })
  
  var nemoTableReferenced:NemoTable = null
  def refToNemoCell(r:String):Option[NemoCell] = nemoTableReferenced(r)
  def apply(name:String) = bindings.get(name).orElse(refToNemoCell(name).flatMap(_.value))
}

//object NormalContext {
//  def apply(c: NemoContext) = new NormalContext(c)
//}

case class NormalContext(precedingContext:NemoContext) extends NemoContext {
  def apply(name:String) = bindings.get(name).orElse(precedingContext(name))
}


// Using Parser Combinators to define syntax/parsing rules of Nemo formulas declaratively.
// Each nemo formula is parsed into an instance of Expr.  Any related utility functions also
// belong here
object NemoParser extends StandardTokenParsers {
  
  override val lexical = ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")", "=", ";", "{", "}", ",", "<", ">")
  lexical.reserved ++= List("let", "fun","true", "false", "if", "else" )
  val numericLiteral = numericLit ^^ {
    i => if (i.contains(".")) ELit(NemoDouble(i.toDouble)) else ELit(NemoInt(i.toInt))
  }
  // takes a parser and a separator and returns a parser that parses a list as a Seq
  def sequencer[T](elementParser:Parser[T], separator:String):Parser[Seq[T]] =
    opt(chainl1(elementParser ^^ {e => Seq(e)}, elementParser, (separator ^^^ {(l:Seq[T], e:T) => l :+ e }))) ^^ {
      case None => Seq[T]()
      case Some(x) => x
    }

  val stringLiteral = stringLit ^^ { s => ELit(NemoString(s)) }
  val booleanLiteral = "true" | "false" 

  def anonFunCall:Parser[Expr] = ("(" ~> expr <~ ")") ~ ("(" ~> exprList <~ ")") ^^ { case f ~ e => EApply(f, e) }
  def funCall:Parser[Expr] = ref ~ ("(" ~> exprList <~ ")") ^^ { case f ~ e => EApply(f, e) }
  val ref = ident ^^ ERef
  def factor:Parser[Expr] =  anonFunCall | funCall | "(" ~> expr <~ ")" | numericLiteral | stringLiteral | ref
  def term = factor * ("*" ^^^ EMul | "/" ^^^ EDiv)
  def subexp = term * ("+" ^^^ EAdd | "-" ^^^ ESub)

  def ifExp = ("if" ~> "(" ~> expr <~ ")") ~ expr ~ ("else" ~> expr) ^^ {
    case cond ~ e1 ~ e2 => EIf(cond, e1, e2)
  }

  def equalExp = (subexp <~ "=") ~ subexp ^^ { case l ~ r => EEq(l, r) }

  def exprList = sequencer(subexp, ",") ^^ { EList(_)}

  def expr:Parser[Expr] = ifExp | funDef | equalExp | subexp // | exprList

  def sLet = ("let" ~> ident <~ "=") ~ expr ^^ {
    case b ~ e => SLet(b,e)
  }

  def sExpr = expr ^^ SExpr
  def stmt = sLet | sExpr

  def stmtBlock = "{" ~> sequencer(stmt, ";") <~ "}"
  def paramList = "(" ~> sequencer(ident, ",") <~ ")"

  def funDef = "fun" ~> paramList ~ stmtBlock ^^ {
    case pl ~ sb => EFun(pl, sb)
  }

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
