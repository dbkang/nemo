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

case class ERef(r:String) extends Expr {
  def eval(c: NemoContext) = c(r)
}

//case class EBody(Seq[Statement]) extends Expr {
//  def eval(c: NemoContext) = {
    

case class EFun(paramList:Seq[String], body:Seq[Statement]) extends Expr {
  def eval(c: NemoContext) = Some(NemoFunction(this, c))
}

object EList {
  def cons(head:Expr, tail:EList) = EList(head +: tail.es)
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

case class EApply(fun:String, args:EList) extends Expr {
  def eval(c:NemoContext) = {
    val cf = c(fun)
    if (cf.isEmpty) {
      for (argsEval <- args.eval(c);
           arg <- argsEval.headOption;
           result <- EApply.functions(fun)(arg)) yield result
      //args.eval.flatMap(a => EApply.functions(fun)(a))
    }
    else if (cf.get.valueType == "Function") {
      val f = cf.get.asInstanceOf[NemoFunction]
      val context = f.context
      val body = f.value.body
      val paramList = f.value.paramList
      if (args.length == paramList.length)
        for (argsEval <- args.eval(c);
             v <- {
               val c = NormalContext(context)
               var lastVal:Option[NemoValue] = None
               c.bindings ++= paramList.zip(argsEval)
               body.foreach {s:Statement => lastVal = s.eval(c)}
               lastVal
             }) yield v
      else Some(NemoError("Wrong # of arguments"))
    }
    else Some(NemoError("Not a function"))
  }
}

trait NemoContext {
  def apply(name:String):Option[NemoValue]
  def add(name:String, value:NemoValue)
}

// this is the base context/scope that simply defines cell references
object NemoPreContext extends NemoContext {
  var nemoTableReferenced:NemoTable = null
  def refToNemoCell(r:String):Option[NemoCell] = nemoTableReferenced(r)
  def apply(name:String) = refToNemoCell(name).flatMap(_.value)
  def add(name: String, value:NemoValue) = ()
}

object NormalContext {
  def apply(c: NemoContext) = new NormalContext(c)
}

class NormalContext(val precedingContext:NemoContext) extends NemoContext {
  val bindings = Map[String, NemoValue]()
  def apply(name:String) = bindings.get(name).orElse(precedingContext(name))
  def add(name: String, value:NemoValue) = {
    bindings += ((name, value))
  }
}
  



// Using Parser Combinators to define syntax/parsing rules of Nemo formulas declaratively.
// Each nemo formula is parsed into an instance of Expr.  Any related utility functions also
// belong here
object NemoParser extends StandardTokenParsers {
  
  override val lexical = ExprLexical
  lexical.delimiters ++= List("+", "-", "*", "/", "(", ")", "=", ";", "{", "}", ",")
  lexical.reserved ++= List("let", "fun")
  val numericLiteral = numericLit ^^ {
    i => if (i.contains(".")) ELit(NemoDouble(i.toDouble)) else ELit(NemoInt(i.toInt))
  }
  // takes a parser and a separator and returns a parser that parses a list as a Seq
  def sequencer[T](elementParser:Parser[T], separator:String):Parser[Seq[T]] =
    chainl1(elementParser ^^ {e => Seq(e)}, elementParser, (separator ^^^ {(l:Seq[T], e:T) => l :+ e }))

  val stringLiteral = stringLit ^^ { s => ELit(NemoString(s)) }
  def funCall:Parser[Expr] = ident ~ ("(" ~> exprList <~ ")") ^^ { case f ~ e => EApply(f, e) }
  val ref = ident ^^ ERef
  def factor:Parser[Expr] =  "(" ~> expr <~ ")" | numericLiteral | stringLiteral | funCall | ref 
  def term = factor * ("*" ^^^ EMul | "/" ^^^ EDiv)
  def subexp = term * ("+" ^^^ EAdd | "-" ^^^ ESub)


  def exprList = sequencer(subexp, ",") ^^ { EList(_)}
//  def exprList = chainl1(subexp ^^ { e:Expr => EList(Seq(e)) }, subexp, ("," ^^^ EList.append _))

  def expr:Parser[Expr] = subexp | exprList | funDef
  def sLet = ("let" ~> ident <~ "=") ~ expr ^^ {
    case b ~ e => SLet(b,e)
  }
  def sExpr = expr ^^ SExpr
  def stmt = sLet | sExpr

  def stmtBlock = "{" ~> sequencer(stmt, ";") <~ "}"
  def paramList = "(" ~> sequencer(ident, ",") <~ ")"
//  def stmtBlock = "{" ~> (stmt ^^ { Seq(_)} ) * (";" ^^^ { (l, s) => l.append(s) }) <~ "}"
//  def paramList = "(" ~> (ident ^^ { Seq(_)} ) * ("," ^^^ { (l:Seq[String], p:String) => l.append(p) }) <~ ")"

  def funDef = "fun" ~> paramList ~ stmtBlock ^^ {
    case pl ~ sb => EFun(pl, sb)
  }

  //def statement = expr <~ ";" | 

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
