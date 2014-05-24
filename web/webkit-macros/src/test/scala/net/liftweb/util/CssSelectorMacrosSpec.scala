package net.liftweb
package util

import org.specs2.mutable.Specification

import common._

object CssSelectorMacrosSpecs extends Specification {
  import CssSelectorMacros._

  "CSS Selector Macros" should {
    "parse top-level selectors" in {
      c"input" must_== ElemSelector("input", Empty)
      c"#magic" must_== IdSelector("magic", Empty)
      c".magic" must_== ClassSelector("magic", Empty)
      c"@magic" must_== NameSelector("magic", Empty)
      c"data-boom=bam" must_== AttrSelector("data-boom", "bam", Empty)
      c"*" must_== StarSelector(Empty, false)
      c"^" must_== StarSelector(Empty, true)
    }

    "parse nested selectors" in {
      c"input #magic .magic @magic data-boom=bam * ^" must_==
        EnclosedSelector(
          ElemSelector("input", Empty),
          EnclosedSelector(
            IdSelector("magic", Empty),
            EnclosedSelector(
              ClassSelector("magic", Empty),
              EnclosedSelector(
                NameSelector("magic", Empty),
                EnclosedSelector(
                  AttrSelector("data-boom", "bam", Empty),
                  EnclosedSelector(
                    StarSelector(Empty, false),
                    StarSelector(Empty, true)
                  )
                )
              )
            )
          )
        )
    }

    "parse child node change selectors" in {
      c"#magic *" must_== IdSelector("magic", Full(KidsSubNode()))
      c"#magic -*" must_== IdSelector("magic", Full(PrependKidsSubNode()))
      c"#magic >*" must_== IdSelector("magic", Full(PrependKidsSubNode()))
      c"#magic *<" must_== IdSelector("magic", Full(AppendKidsSubNode()))
      c"#magic *+" must_== IdSelector("magic", Full(AppendKidsSubNode()))
      c"#magic <*>" must_== IdSelector("magic", Full(SurroundKids()))
      c"#magic <*>" must_== IdSelector("magic", Full(SurroundKids()))
    }

    "parse don't-merge-attributes selector" in {
      c"#magic !!" must_== IdSelector("magic", Full(DontMergeAttributes))
    }

    "parse and mark attribute change selectors" in {
      c"#magic [onclick]" must_== AttrModifyingSelector("onclick", IdSelector("magic", Full(AttrSubNode("onclick"))))
      c"#magic [onclick+]" must_== AttrModifyingSelector("onclick", IdSelector("magic", Full(AttrAppendSubNode("onclick"))))
      c"#magic [onclick!]" must_== AttrModifyingSelector("onclick", IdSelector("magic", Full(AttrRemoveSubNode("onclick"))))
    }

    "parse select-this-node selectors" in {
      c"#magic ^*" must_== IdSelector("magic", Full(SelectThisNode(true)))
      c"#magic ^^" must_== IdSelector("magic", Full(SelectThisNode(false)))
    }
  }
}
