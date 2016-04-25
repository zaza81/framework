package net.liftweb.util

import org.specs2.mutable.Specification

import scala.xml.Text

object VDomCompareSpec extends Specification {
  "VDom.compare()".title

  "VDom.compare" should {
    import VDom.compare

    "regard elements with different tags as dissimilar" in {
      val a = <div></div>
      val b = <span></span>
      compare(a, b) must_== 0f
    }

    "regard elements with different tags as dissimilar even if have same id" in {
      val a = <div id="nope"></div>
      val b = <span id="nope"></span>
      compare(a, b) must_== 0f
    }

    "regard elements with the same tags and children as the same" in {
      val a = <span>Some text</span>
      val b = <span>Some text</span>  // Purposefully making a copy for the test
      compare(a, b) must_== 1f
    }

    "regard two Text nodes with the same text as the same" in {
      val a = Text("text")
      val b = Text("text")
      compare(a, b) must_== 1f
    }

    "regard two Text nodes with different text as different" in {
      val a = Text("text")
      val b = Text("blah")
      compare(a, b) must_== 0f
    }

    "regard elements with the same tags and no children as the same" in {
      val a = <div></div>
      val b = <div></div>
      compare(a, b) must_== 1f
    }

    "regard elements with the same tags but different children as a ratio of similar children" in {
      val a =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Three</li>
        </ul>

      compare(a, b) must_== 0.5f
    }

    "regard elements with the same tags but different number children as a ratio of similar children counting absentees as dissimilar" in {
      val a =
        <ul>
          <li>One</li>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>

      compare(a, b) must_== 0.5f
    }

    "regard elements with the same tags but one with children and one without as dissimilar" in {
      val a =
        <ul>
        </ul>
      val b =
        <ul>
          <li>One</li>
          <li>Two</li>
        </ul>

      compare(a, b) must_== 0.0f
    }

    "regard elements with same tags and ids as the same regardless of children" in {
      val a = <span id="same">Not</span>
      val b = <span id="same">Different</span>
      compare(a, b) must_== 1f
    }
  }


}
