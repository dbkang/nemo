import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.OptionValues


class NemoTableSpec extends FunSpec with ShouldMatchers with BeforeAndAfter with OptionValues {
  var nt = NemoTable(512, 64)

  before {

  }

  after {
    nt = NemoTable(512,64)
  }

  describe("NemoTable") {
    it("should be able to store formulas in a NemoCell and return a NemoCell via value method") {
      nt.setFormula(1, 1, "(asdfdfdf")
      nt.value(1, 1).value.formula should be ("(asdfdfdf")
    }

    it("should be able to set formulas using model.setValueAt and undo and redo those actions") {
      nt.model.setValueAt("do", 1, 1)
      nt.value(1,1).value.formula should be ("do")
      nt.model.setValueAt("re", 1, 1)
      nt.value(1,1).value.formula should be ("re")
      nt.model.setValueAt("mi", 1, 1)
      nt.value(1,1).value.formula should be ("mi")
      nt.undo
      nt.value(1,1).value.formula should be ("re")
      nt.undo
      nt.value(1,1).value.formula should be ("do")
      nt.undo
      nt.value(1,1).value.formula should be ("")
      nt.redo
      nt.value(1,1).value.formula should be ("do")
      nt.redo
      nt.value(1,1).value.formula should be ("re")
      nt.model.setValueAt("solatido", 1, 1)
      nt.value(1,1).value.formula should be ("solatido")
      nt.redo
      nt.value(1,1).value.formula should be ("solatido")
      nt.undo
      nt.value(1,1).value.formula should be ("re")
    }

    it("should correctly resolve addresses in the A1 form and return the correct NemoCell") {
      nt.model.setValueAt("doremi", 0,0)
      nt("a1").value.formula should be ("doremi")
      nt("b1").value.formula should be ("")
      nt.model.setValueAt("doremifa",1,0)
      nt("a2").value.formula should be ("doremifa")
    }

    it("should correctly serialize to and deserialize from XML") {
      nt.model.setValueAt("doremi",0,0)
      nt.model.setValueAt("fasola",1,0)
      nt.model.setValueAt("tido",125,25)
      val saved = nt.toNodeSeq
      val rowCount = nt.model.getRowCount
      val colCount = nt.model.getColumnCount
      val nt2 = NemoTable(saved)
      nt2("a1").value.formula should be ("doremi")
      nt2("a2").value.formula should be ("fasola")
      nt2("z126").value.formula should be ("tido")
      nt2.model.getRowCount should be (rowCount)
      nt2.model.getColumnCount should be (colCount)

    }
  }
}
