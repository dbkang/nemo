import org.scalatest.FunSuite

class NemoParserTest extends FunSuite {
  def eval(a:String) = NemoParser(a).get.eval(NemoPreContext).get.value

  test("Numeric literals are parsed and evaluated to be themselves") {
    assert(eval("54234") === 54234)
    assert(eval("54.234") === 54.234)
    assert(eval(".234") === 0.234)
  }

  test("Numeric binary operations are parsed and evaluated correctly") {
    assert(eval("54+25") === 79)
    assert(eval("54-24") === 30)
    assert(eval("5+2*2") === 9)
    assert(eval("5+2/2") === 6)
    assert(eval("(5+2)*2") === 14)
    assert(eval("(6+2)/2") === 4)
    assert(eval("(6.0+2.0)*2.0") === 16)
    assert(eval("(6.0+2.0)*2.0 == 16.0*1") === true)
  }

  test("Boolean literals and operations are parsed and evaluated correctly") {
    assert(eval("true") === true)
    assert(eval("false") === false)
    assert(eval("true || false") === true)
    assert(eval("false || true") == true)
    assert(eval("true || true") === true)
    assert(eval("false || false") === false)
    assert(eval("true && false") === false)
    assert(eval("false && true") == false)
    assert(eval("true && true") === true)
    assert(eval("false && false") === false)
    assert(eval("5 == 5 || 4 == 10") === true)
    assert(eval("5 == 5 && 4 == 10") === false)
  }

  test("String literals and operations involving them are parsed and evaluated correctly") {
    assert(eval("\"yoyoma\"") === "yoyoma")
    assert(eval("\"yoyoma\"+123") === "yoyoma123")
    assert(eval("\"yoyoma\"+\"123\"") === "yoyoma123")
    assert(eval("\"\"+\"123\"") === "123")
    assert(eval("\"believe\"+\"123\" == \"believe123\"") === true)
  }

  test("Functions and references are parsed and evaluated correctly") {
    assert(eval("(fun (a,b) { a(b) })(fun (a) { a + 5 }, 10)") === 15)
    assert(eval("(fun () { 5 * 5 == 25 })()") === true)
    assert(eval("(fun (a) { a() })(fun () { \"somestring\" })") === "somestring")
  }

  test("Non-boolean types are properly coerced into booleans") {
    assert(eval("0 && true") === false)
    assert(eval("25 && true") === true)
    assert(eval("\"\" && true") === false)
    assert(eval("\"dude\" && true") === true)
    assert(eval("nil && true") === false)
    assert(eval("list() && true") === false)
    assert(eval("list(1,2,3) && true") === true)
  }

  test("Variables bound by let statement are correctly evaluated") {
    assert(eval("(fun (a, b) { let c = a + b; c + 10 })(5,5)") === 20)
    assert(eval("(fun (a, b) { let a = b; a + 10 })(1,10)") === 20)
  }

  test("Dot notation for function/method calling is parsed and applied correctly") {
    assert(eval("5.add(5)") === 10)
    assert(eval("typeof.typeof") === "Primitive")
    assert(eval("5.typeof()") === "Int")
    assert(eval("5.cons(10).head") === 5)
    assert(eval("typeof.typeof.typeof()") === "String")
    assert(eval("5.image(10).typeof") === "Image")
  }
}
