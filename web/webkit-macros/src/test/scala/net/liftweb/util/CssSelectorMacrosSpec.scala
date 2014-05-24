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

    "parse change selectors across different selector types" in {
      // Huge set of assertions here, but we are verifying that all types of
      // selectors will correctly read their subnodes. We have to do this
      // because the c macro has to explicitly treeify all of the selector
      // results, and we need to check that it does all of that right.
      c"input *" must_== ElemSelector("input", Full(KidsSubNode()))
      c"input -*" must_== ElemSelector("input", Full(PrependKidsSubNode()))
      c"input >*" must_== ElemSelector("input", Full(PrependKidsSubNode()))
      c"input *<" must_== ElemSelector("input", Full(AppendKidsSubNode()))
      c"input *+" must_== ElemSelector("input", Full(AppendKidsSubNode()))
      c"input <*>" must_== ElemSelector("input", Full(SurroundKids()))
      c"input <*>" must_== ElemSelector("input", Full(SurroundKids()))
      c"input !!" must_== ElemSelector("input", Full(DontMergeAttributes))
      c"input [onclick]" must_== AttrModifyingSelector("onclick", ElemSelector("input", Full(AttrSubNode("onclick"))))
      c"input [onclick+]" must_== AttrModifyingSelector("onclick", ElemSelector("input", Full(AttrAppendSubNode("onclick"))))
      c"input [onclick!]" must_== AttrModifyingSelector("onclick", ElemSelector("input", Full(AttrRemoveSubNode("onclick"))))
      c"input ^*" must_== ElemSelector("input", Full(SelectThisNode(true)))
      c"input ^^" must_== ElemSelector("input", Full(SelectThisNode(false)))

      c".magic *" must_== ClassSelector("magic", Full(KidsSubNode()))
      c".magic -*" must_== ClassSelector("magic", Full(PrependKidsSubNode()))
      c".magic >*" must_== ClassSelector("magic", Full(PrependKidsSubNode()))
      c".magic *<" must_== ClassSelector("magic", Full(AppendKidsSubNode()))
      c".magic *+" must_== ClassSelector("magic", Full(AppendKidsSubNode()))
      c".magic <*>" must_== ClassSelector("magic", Full(SurroundKids()))
      c".magic <*>" must_== ClassSelector("magic", Full(SurroundKids()))
      c".magic !!" must_== ClassSelector("magic", Full(DontMergeAttributes))
      c".magic [onclick]" must_== AttrModifyingSelector("onclick", ClassSelector("magic", Full(AttrSubNode("onclick"))))
      c".magic [onclick+]" must_== AttrModifyingSelector("onclick", ClassSelector("magic", Full(AttrAppendSubNode("onclick"))))
      c".magic [onclick!]" must_== AttrModifyingSelector("onclick", ClassSelector("magic", Full(AttrRemoveSubNode("onclick"))))
      c".magic ^*" must_== ClassSelector("magic", Full(SelectThisNode(true)))
      c".magic ^^" must_== ClassSelector("magic", Full(SelectThisNode(false)))

      c"@magic *" must_== NameSelector("magic", Full(KidsSubNode()))
      c"@magic -*" must_== NameSelector("magic", Full(PrependKidsSubNode()))
      c"@magic >*" must_== NameSelector("magic", Full(PrependKidsSubNode()))
      c"@magic *<" must_== NameSelector("magic", Full(AppendKidsSubNode()))
      c"@magic *+" must_== NameSelector("magic", Full(AppendKidsSubNode()))
      c"@magic <*>" must_== NameSelector("magic", Full(SurroundKids()))
      c"@magic <*>" must_== NameSelector("magic", Full(SurroundKids()))
      c"@magic !!" must_== NameSelector("magic", Full(DontMergeAttributes))
      c"@magic [onclick]" must_== AttrModifyingSelector("onclick", NameSelector("magic", Full(AttrSubNode("onclick"))))
      c"@magic [onclick+]" must_== AttrModifyingSelector("onclick", NameSelector("magic", Full(AttrAppendSubNode("onclick"))))
      c"@magic [onclick!]" must_== AttrModifyingSelector("onclick", NameSelector("magic", Full(AttrRemoveSubNode("onclick"))))
      c"@magic ^*" must_== NameSelector("magic", Full(SelectThisNode(true)))
      c"@magic ^^" must_== NameSelector("magic", Full(SelectThisNode(false)))

      c"data-boom=bam *" must_== AttrSelector("data-boom", "bam", Full(KidsSubNode()))
      c"data-boom=bam -*" must_== AttrSelector("data-boom", "bam", Full(PrependKidsSubNode()))
      c"data-boom=bam >*" must_== AttrSelector("data-boom", "bam", Full(PrependKidsSubNode()))
      c"data-boom=bam *<" must_== AttrSelector("data-boom", "bam", Full(AppendKidsSubNode()))
      c"data-boom=bam *+" must_== AttrSelector("data-boom", "bam", Full(AppendKidsSubNode()))
      c"data-boom=bam <*>" must_== AttrSelector("data-boom", "bam", Full(SurroundKids()))
      c"data-boom=bam <*>" must_== AttrSelector("data-boom", "bam", Full(SurroundKids()))
      c"data-boom=bam !!" must_== AttrSelector("data-boom", "bam", Full(DontMergeAttributes))
      c"data-boom=bam [onclick]" must_== AttrModifyingSelector("onclick", AttrSelector("data-boom", "bam", Full(AttrSubNode("onclick"))))
      c"data-boom=bam [onclick+]" must_== AttrModifyingSelector("onclick", AttrSelector("data-boom", "bam", Full(AttrAppendSubNode("onclick"))))
      c"data-boom=bam [onclick!]" must_== AttrModifyingSelector("onclick", AttrSelector("data-boom", "bam", Full(AttrRemoveSubNode("onclick"))))
      c"data-boom=bam ^*" must_== AttrSelector("data-boom", "bam", Full(SelectThisNode(true)))
      c"data-boom=bam ^^" must_== AttrSelector("data-boom", "bam", Full(SelectThisNode(false)))

      c"* *" must_== StarSelector(Full(KidsSubNode()), false)
      c"* -*" must_== StarSelector(Full(PrependKidsSubNode()), false)
      c"* >*" must_== StarSelector(Full(PrependKidsSubNode()), false)
      c"* *<" must_== StarSelector(Full(AppendKidsSubNode()), false)
      c"* *+" must_== StarSelector(Full(AppendKidsSubNode()), false)
      c"* <*>" must_== StarSelector(Full(SurroundKids()), false)
      c"* <*>" must_== StarSelector(Full(SurroundKids()), false)
      c"* !!" must_== StarSelector(Full(DontMergeAttributes), false)
      c"* [onclick]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrSubNode("onclick")), false))
      c"* [onclick+]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrAppendSubNode("onclick")), false))
      c"* [onclick!]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrRemoveSubNode("onclick")), false))
      c"* ^*" must_== StarSelector(Full(SelectThisNode(true)), false)
      c"* ^^" must_== StarSelector(Full(SelectThisNode(false)), false)

      c"^ *" must_== StarSelector(Full(KidsSubNode()), true)
      c"^ -*" must_== StarSelector(Full(PrependKidsSubNode()), true)
      c"^ >*" must_== StarSelector(Full(PrependKidsSubNode()), true)
      c"^ *<" must_== StarSelector(Full(AppendKidsSubNode()), true)
      c"^ *+" must_== StarSelector(Full(AppendKidsSubNode()), true)
      c"^ <*>" must_== StarSelector(Full(SurroundKids()), true)
      c"^ <*>" must_== StarSelector(Full(SurroundKids()), true)
      c"^ !!" must_== StarSelector(Full(DontMergeAttributes), true)
      c"^ [onclick]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrSubNode("onclick")), true))
      c"^ [onclick+]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrAppendSubNode("onclick")), true))
      c"^ [onclick!]" must_== AttrModifyingSelector("onclick", StarSelector(Full(AttrRemoveSubNode("onclick")), true))
      c"^ ^*" must_== StarSelector(Full(SelectThisNode(true)), true)
      c"^ ^^" must_== StarSelector(Full(SelectThisNode(false)), true)
    }
  }
}
