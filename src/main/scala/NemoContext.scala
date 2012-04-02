import scala.collection.mutable.Map
import scala.collection.immutable.PagedSeq
import javax.imageio.ImageIO
import java.net.URL
import scala.io.Source

// Context/scope object
abstract class NemoContext {
  def apply(name:String):Option[NemoValue]
  def add(name:String, value:NemoValue) {}
}

trait MutableContext extends NemoContext {
  val bindings = Map[String, NemoValue]()
  override def add(name: String, value:NemoValue) = {
    bindings += ((name, value))
  }
}

// this is the base context/scope that define cell references, loads
// primitives and standard library
case object NemoPreContext extends NemoContext with MutableContext {
  def addPrimitive(name:String, function:NemoList=>Option[NemoValue]) = {
    bindings += ((name, NemoPrimitive(name, function)))
  }
  def standardLib = Source.fromURL(getClass.getResource("/standard.ns"))

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

    NemoParser.parseSourceFile(standardLib).map {
      l => l.foreach { _.eval(this) }
    }
  }

  load
  def apply(name:String) = bindings.get(name)
}


case class NemoSheetContext(precedingContext:NemoContext, sheetModel:NemoSheetModel) extends NemoContext {
  def apply(name:String) = sheetModel(name).flatMap(_.value).orElse(precedingContext(name))
}

case class NormalContext(precedingContext:NemoContext) extends NemoContext with MutableContext{
  def apply(name:String) = bindings.get(name).orElse(precedingContext(name))
}
