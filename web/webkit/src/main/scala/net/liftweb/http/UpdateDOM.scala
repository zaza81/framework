package net.liftweb.http

import net.liftweb.common.{Full, Empty, Box}
import net.liftweb.http.js.JsCmd
import net.liftweb.json.Serialization
import net.liftweb.util.VDom

import scala.xml.Node

// TODO: Change to Box[() => VNode] to perform Node => VNode conversion only once
private [http] object lastRender extends RequestVar[Box[Node]](Empty)
private [http] object nextRender extends RequestVar[Box[() => Node]](Empty)

class UpdateDOM extends JsCmd {
  implicit val formats = VDom.formats

  override def toJsCmd:String = {
    val vDomUpdate = for {
      s <- S.session
      last <- lastRender.get
      nextF <- nextRender.get
      next = nextF()
      lastBody <- (last \\ "body").headOption
      nextBody <- (next \\ "body").headOption
    } yield {
      lastRender.set(Full(next))
      val diffObj = VDom.diff(lastBody, nextBody)
      val diffStr = Serialization.write(diffObj)

      println(diffStr)

      s"lift.updateBody($diffStr);"
    }

    vDomUpdate.openOr("")
  }

}
