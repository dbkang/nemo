// General helper methods to easily call code outside of EDT

import akka.actor.Actor
import akka.actor.ActorRef
import scala.swing.Reactor
import scala.swing.Publisher
import scala.swing.event.Event
import scala.collection.mutable.HashMap

object EDTCallback {
  def apply(exp: =>Unit) {
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run = exp
    })
    //println("Back on the EDT baby")
  }
}

class SwingContractor(work: =>Unit) extends Actor {
  def receive = {
    case _ => {
      work
      EDTCallback(()=>println("I'm working bro"))
    }
  }
}

class SwingEmployee extends Actor {
  def receive = {
    case DoThis(w) => w
  }
}

case class DoWork(contractor:String) extends Event
case class DoThis(work: ()=>Unit)

object SwingGeneralContractor extends Publisher {
  val subcontractors = HashMap[String, ActorRef]()

  def createSubContractor(contractor: String, runNow:Boolean, customer:Publisher = this)(work: =>Unit) = {
    val sc = Actor.actorOf(new SwingContractor(work))
    listenTo(customer)
    sc.start
    subcontractors += ((contractor, sc))
    if (runNow) sc ! Unit
  }

  def doOneOff = Actor.spawn _

  def doWork(contractor:String) = {
    subcontractors(contractor) ! Unit
  }

  reactions += {
    case DoWork(x) => subcontractors(x) ! Unit
  }
}

object TestSwingContractor extends App {
//  SwingGeneralContractor.createSubContractor("tommy", true)(()=>println("Tommy's got his six-string in hock"))
//  SwingGeneralContractor.doOneOff(()=>println("Now he's holding in"))
//  SwingGeneralContractor.doWork("tommy")
//  Actor.spawn {
//    println("heyheyhey")
//  }
}
