import org.scalatest.FunSuite
import org.scalatest.OptionValues

class NemoContextTest extends FunSuite with OptionValues {
  val pre = NemoPreContext("")
  test("NemoPreContext includes primitives as added") {
    assert(pre("nil").value === NemoUnit)
    assert(pre("image").value.valueType === "Primitive")
    assert(pre("url").value.valueType === "Primitive")
    assert(pre("resize").value.valueType === "Primitive")
    assert(pre("command").value.valueType === "Primitive")
    assert(pre("apply").value.valueType === "Primitive")
    assert(pre("cons").value.valueType === "Primitive")
    assert(pre("head").value.valueType === "Primitive")
    assert(pre("tail").value.valueType === "Primitive")
    assert(pre("typeof").value.valueType === "Primitive")
  }

  test("NemoContext can add bindings") {
    val v = NemoString("hello")
    val nc = NormalContext(pre)
    nc.add("testbinding", v)
    assert(nc("testbinding").value === v)
  }

  test("NemoContext can access preceding context's bindings") {
    val nc = NormalContext(pre)
    assert(nc("tail").value.valueType === "Primitive")
    assert(nc("typeof").value.valueType === "Primitive")
  }
}

