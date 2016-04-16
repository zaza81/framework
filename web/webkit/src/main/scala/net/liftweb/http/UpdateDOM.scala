package net.liftweb.http

import net.liftweb.common.{Empty, Box}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop

import scala.xml.Node

private [http] object lastRender      extends RequestVar[Box[Node]](Empty)
private [http] object nextRender      extends RequestVar[Box[() => Node]](Empty)
private [http] object updateDomComet  extends RequestVar[Box[CometActor]](Empty)

object SendUpdateDOM {
  def apply(andThen:JsCmd = Noop):Unit = {
    updateDomComet.get.foreach(_ ! UpdateDOM(andThen))
  }
}
case class UpdateDOM(andThen:JsCmd)
