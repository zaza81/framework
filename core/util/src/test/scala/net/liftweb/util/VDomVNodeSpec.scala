package net.liftweb.util

import VDom.VNode

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

object VDomVNodeSpec extends Specification with XmlMatchers {
  "VDom.VNode Specification".title

  "VNode.fromXml" should {
    import VNode.fromXml

    "create a VNode from a typical html sample" in {
      val html =
        <form method="post" data-lift="form.ajax">
          <span>Enter your stuff:</span>
          <hr/>
          <div data-lift="Chat.submit">
            <input type="text" id="chat-in" name="in"/>
            <input type="submit" value="Submit"/>
          </div>
        </form>

      val vdom = VNode("form", Map("method" -> "post", "data-lift" -> "form.ajax"), List(
        VNode("span", Map(), List(VNode.text("Enter your stuff:"))),
        VNode("hr", Map(), List()),
        VNode("div", Map("data-lift" -> "Chat.submit"), List(
          VNode("input", Map("type" -> "text", "id" -> "chat-in", "name" -> "in")),
          VNode("input", Map("type" -> "submit", "value" -> "Submit")
        ))
      )))

      fromXml(html) must_== vdom
    }
  }
}
