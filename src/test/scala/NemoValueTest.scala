import org.scalatest.FunSuite

class NemoValueTest extends FunSuite {

  test("NemoInt binary operations work properly") {
    assert(NemoInt(5) + NemoInt(4) === NemoInt(9))
    assert(NemoInt(5) * NemoInt(4) === NemoInt(20))
    assert(NemoInt(5) - NemoInt(4) === NemoInt(1))
    assert(NemoInt(8) / NemoInt(4) === NemoInt(2))
  }

  test("NemoValue coercions work properly") {
    assert(NemoDouble(5).toIntOption.get === 5)
    assert(NemoInt(5).toDoubleOption.get === 5.0)
    assert(NemoInt(5).toString === "5")
    assert(NemoDouble(5).toString === "5.0")
    assert(NemoDouble(0.0).toBoolean === false)
    assert(NemoDouble(6.0).toBoolean === true)
    assert(NemoString("").toBoolean === false)
    assert(NemoString("Jim").toBoolean === true)
  }

  test("+ works as append for NemoStrings") {
    assert(NemoString("dude") + NemoString("yo") === NemoString("dudeyo"))
    assert(NemoString("cool") + NemoInt(12345) === NemoString("cool12345"))
    assert(NemoString("surething") + NemoDouble(5.5) === NemoString("surething5.5"))  
    assert(NemoString("me") + NemoBoolean(true) === NemoString("metrue"))
  }

  test("NemoCons operations work") {
    val pair = NemoCons(NemoInt(5), NemoString("yo"))
    val list = NemoList(List(NemoInt(5), NemoString("see"), NemoDouble(5.5)))
    assert(pair.headOption.get === NemoInt(5))
    assert(pair.tailOption.get === NemoString("yo"))
    assert(list.toSeqOption.get(0) === NemoInt(5))
    assert(list.toSeqOption.get(1) === NemoString("see"))
    assert(list.toSeqOption.get(2) === list(2).get)
  }
    

  test("Operations for incompatible types result in NemoError") {
    assert((NemoInt(5) + NemoString("Jim")).valueType === "Error")
    assert((NemoBoolean(true) + NemoString("Jim")).valueType === "Error")
    assert((NemoString("Jimbo") / NemoString("Jim")).valueType === "Error")
    assert((NemoCons(NemoInt(5), NemoInt(3)) * NemoInt(2)).valueType === "Error")
  }
}
