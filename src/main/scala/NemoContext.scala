import scala.collection.mutable.Map
import scala.collection.immutable.PagedSeq
import javax.imageio.ImageIO
import java.net.URL
import scala.io.Source

// Context/scope object
abstract class NemoContext {
  val bindings = Map[String, NemoValue]()
  def apply(name:String):Option[NemoValue]
  def add(name: String, value:NemoValue) = {
    bindings += ((name, value))
  }
}

// this is the base context/scope that define cell references, loads
// primitives and standard library
case object NemoPreContext extends NemoContext {
  def addPrimitive(name:String, function:NemoList=>Option[NemoValue]) = {
    bindings += ((name, NemoPrimitive(name, function)))
  }
  def load = {
    bindings.clear

    bindings += (("nil", NemoList.nil))
    
    addPrimitive("url", args => {
      args.headOption.map(v => {
        NemoImage(ImageIO.read(new URL(v.toString)), true)      
      })
    })
    
    addPrimitive("image", args => {
      for (arg1 <- args(0);
           arg2 <- args(1);
           width <- arg1.toIntOption;
           height <- arg2.toIntOption)
      yield NemoImage(width,height)
    })
    
    
    addPrimitive("resize", args => {
      for (arg1 <- args(0);
           arg2 <- args(1);
           arg3 <- args(2);
           image <- arg1.toImageOption;
           width <- arg2.toIntOption;
           height <- arg3.toIntOption)
      yield NemoImage(image).resize(width,height)
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
    
    addPrimitive("apply", args => {
      for
        ((f, fargs) <- (args.headOption, args.tailOption) match {
          case (Some(f1:NemoFunction), Some(NemoCons(h:NemoList, t))) => Some((f1, h))
          case _ => None };
         v <- f(fargs)) yield v
    })
    
    addPrimitive("cons", args => {
      for (head <- args(0);
           tail <- args(1))
      yield NemoCons(head,tail)
    })
    
    addPrimitive("head", args => {
      args(0) match {
        case Some(NemoCons(h, _)) => Some(h)
        case _ => None
      }
    })
    
    addPrimitive("tail", args => {
      args(0) match {
        case Some(NemoCons(_, t)) => Some(t)
        case _ => None
      }
    })
    
    addPrimitive("typeof", args => {
      args(0).map { v => NemoString(v.valueType) }
    })

    NemoParser.parseSourceFile(Source.fromURL(getClass.getResource("/standard.ns"))).map {
      l => l.foreach { _.eval(this) }
    }
  }

  load
  var nemoTableReferenced:NemoTable = null
  def refToNemoCell(r:String):Option[NemoCell] = {
    if (nemoTableReferenced == null)
      None
    else
      nemoTableReferenced(r)
  }
  def apply(name:String) = bindings.get(name).orElse(refToNemoCell(name).flatMap(_.value))
}

//object NormalContext {
//  def apply(c: NemoContext) = new NormalContext(c)
//}

case class NormalContext(precedingContext:NemoContext) extends NemoContext {
  def apply(name:String) = bindings.get(name).orElse(precedingContext(name))
}
