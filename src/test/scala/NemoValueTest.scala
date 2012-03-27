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

}
