//> using lib "org.scalameta::munit::0.7.29"
//> using scala "2.13"
//> using publish.url "https://github.com"

class MyTests extends munit.FunSuite {
  test("foo") {
    assert(2 + 2 == 4)
  }
  // test("nope") {
  //   assert(2 + 2 == 5)
  // }
}
