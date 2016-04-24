package net.liftweb.util

import net.liftweb.util.VDom.VDomHelpers._
import net.liftweb.util.VDom.{VNodeReorder, VNodeDelete, VNode, VNodeInsert}
import VNode.{text => txt}
import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

object VDomSpec extends Specification with XmlMatchers {
  "VDom Specification".title

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

  "VDomHelpers.nodeCount" should {
    "count the number of nodes in an XML tree" in {
      val xml = <a>
        <b></b>
        <c></c>
        <d>
          <e><f></f></e>
          <g></g>
        </d>
        H
        <i>J</i>
      </a>

      nodeCount(xml) must_== 10
    }
  }

  "VDomHelpers.insertNode" should {
    "insert a node after the first child" in {
      val before = <div><span></span></div>
      val toAdd = <span>text</span>
      val after = <div><span></span>{toAdd}</div>

      insertNode(before, toAdd, 1, true) must beEqualToIgnoringSpace(after)
    }

    "insert a node after the second child" in {
      val before = <div><span></span><div></div></div>
      val toAdd = <span>text</span>
      val after = <div><span></span><div></div>{toAdd}</div>

      insertNode(before, toAdd, 2, true) must beEqualToIgnoringSpace(after)
    }

    "insert a grandchild node after another" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val toAdd = <li>msg 4</li>

      val after = <div>
        <ul>
          <li>msg 1</li>
          {toAdd}
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      insertNode(before, toAdd, 4, true) must beEqualToIgnoringSpace(after)
    }
  }

  "VDomHelpers.recFilter" should {
    "filter all whitespace nodes when passed isntWhitespace" in {
      val before = <div>
        <ul>
          <li>msg 1</li>
          <li>msg 2</li>
          <li>msg 3</li>
        </ul>
      </div>

      val after = <div><ul><li>msg 1</li><li>msg 2</li><li>msg 3</li></ul></div>

      recFilter(before, isntWhitespace) must_== after
    }
  }

  "VDom.compare" should {
    import VDom.compare

    "regard elements with different tags as dissimilar" in {
      compare(<div></div>, <span></span>) must_== 0f
    }

    "regard elements with the same tags and children as the same" in {
      val a = <span>Some text</span>
      val b = <span>Some text</span>  // Purposefully making a copy for the test
      compare(a, b) must_== 1f
    }
  }

  "VDom.diff" should {
    import VDom.diff

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

      diff(before, after) must_== expected
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

      diff(before, after) must_== expected
    }.pendingUntilFixed("Not doing removes yet")

    "find reordered elements" in {
      val before =
        <div>
          <hr/>
          <ul>
            <li>Message 1</li>
            <li>Message 2</li>
            <li>Message 3</li>
            <li>Message 4</li>
          </ul>
        </div>

      val after =
        <div>
          <hr/>
          <ul>
            <li>Message 2</li>
            <li>Message 4</li>
            <li>Message 3</li>
            <li>Message 1</li>
          </ul>
        </div>

      val expected =
        node(
          node(),
          node(
            node(),
            node()
          ).withTransforms(VNodeReorder(Map(0 -> 3, 1 -> 0, 3 -> 1)))
        )

      diff(before, after) must_== expected
    }.pendingUntilFixed

  }
}
