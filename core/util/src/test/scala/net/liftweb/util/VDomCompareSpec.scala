package net.liftweb.util

import org.specs2.mutable.Specification

object VDomCompareSpec extends Specification {
  "VDom.compare()".title

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


}
