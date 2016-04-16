package net.liftweb.http

import net.liftweb.common.{Empty, Box}
import net.liftweb.http.js.JsCmd

import scala.xml.Node

private [http] object lastRender      extends RequestVar[Box[Node]](Empty)
private [http] object nextRender      extends RequestVar[Box[() => Node]](Empty)
private [http] object updateDomComet  extends RequestVar[Box[CometActor]](Empty)

case object UpdateDOM extends JsCmd {
  override def toJsCmd:String = {
    updateDomComet.get.foreach(_ ! this)
    ""
  }
}
