package net.liftweb.util

import VDom._
import VDomHelpers._
import net.liftweb.json.Extraction._

import org.specs2.mutable.Specification

object VDomFormatsSpec extends Specification {
  "VDom Formats Specification".title

  implicit val f = formats

  def check(patch:VNodeTransform) = extract[VNodeTransform](decompose(patch)) must_== patch

  "VDom.formats" should {
    "round-trip a typical VNodeInsert" in {
      val patch = VNodeInsert(5, VNode("div", Map("class" -> "blah"), List(
        VNode("span", children = List(
          text("stuff")
        )),
        VNode("p")
      )))

      check(patch)
    }

    "round-trip a typical VNodeDelete" in {
      val patch = VNodeDelete(20)

      check(patch)
    }

    "round-trip a typical VNodeReorder" in {
      val patch = VNodeReorder(0, 2, 3)

      check(patch)
    }
  }
}
