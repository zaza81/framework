package net.liftweb.http

import net.liftweb.common.{Full, Empty, Box}
import net.liftweb.http.js.JsCmd

import scala.xml.Node

// TODO: Change to Box[() => VNode] to perform Node => VNode conversion only once
private [http] object lastRender extends RequestVar[Box[Node]](Empty)
private [http] object nextRender extends RequestVar[Box[() => Node]](Empty)

class UpdateDOM extends JsCmd {
  override def toJsCmd:String = {
    val vDomUpdate = for {
      s <- S.session
      last <- lastRender.get
      nextF <- nextRender.get
    } yield {
      val next:Node = nextF()
      lastRender.set(Full(next))

      println("Calculating VDOM for:")
      println(next)

      "lift.logError('hello');"
    }

    vDomUpdate.openOr("")
  }

}
