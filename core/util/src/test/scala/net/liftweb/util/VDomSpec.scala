package net.liftweb.util

import net.liftweb.util.VDom.VDomHelpers._
import net.liftweb.util.VDom.{VNodeDelete, VNode, VNodeInsert}
import VNode.{text => txt}
import org.specs2.mutable.Specification

object VDomSpec extends Specification {
  "VDom Specification".title

  "VNode.fromXml" should {
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

      VNode.fromXml(html) must_== vdom
    }
  }

  "VDom.diff" should {
    "find an added element" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 3</li>
          </ul>
        </div>

      val expected =
        node(
          node(),
          node(
            node(),
            node()
          ).withTransforms(VNodeInsert(2, VNode("li", Map(), List(txt("Message 3")))))
        )

      VDom.diff(before, after) must_== expected
    }

    "find an removed element" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
          </ul>
        </div>

      val expected =
        node(
          node(),
          node(
            node()
          ).withTransforms(VNodeDelete(0))
        )

      VDom.diff(before, after) must_== expected
    }.pendingUntilFixed("Not doing removes yet")
  }
}
