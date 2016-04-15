package net.liftweb.util.vdom

import org.specs2.mutable.Specification
import VDomHelpers._
import VDomHelpers.{ text => txt }

object VDomSpec extends Specification {
  "VDom Specification".title

  "VNode.fromXml" should {
    "a VNode from a typical html sample" in {
      val html =
        <form method="post" data-lift="form.ajax">
          <span>Enter your stuff:</span>
          <div data-lift="Chat.submit">
            <input type="text" id="chat-in" name="in"/>
            <input type="submit" value="Submit"/>
          </div>
        </form>

      val vdom = VNode("form", Map("method" -> "post", "data-lift" -> "form.ajax"), List(
        VNode("span", Map(), List(VNode.text("Enter your stuff:"))),
        VNode("div", Map("data-lift" -> "Chat.submit"), List(
          VNode("input", Map("type" -> "text", "id" -> "chat-in", "name" -> "in")),
          VNode("input", Map("type" -> "submit", "value" -> "Submit")
        ))
      )))

      VNode.fromXml(html) must_== vdom
    }
  }

  "VDom.diff" should {
    "find an added <li>" in {
      val before =
        <p>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </p>

      val after =
        <p>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 3</li>
          </ul>
        </p>

      val expected =
        node(
          node().withTransforms(VNodeInsert(2, VNode("li", Map(), List(txt("Message 3")))))
        )

      VDom.diff(before, after) must_== expected
    }
  }
}
