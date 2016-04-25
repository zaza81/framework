package net.liftweb.util

import VDom.VDomHelpers._

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

object VDomHelpersSpec extends Specification with XmlMatchers {
  "VDomHelpers Specification".title

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

}
