package com.nik.lab

import munit.FunSuite

class MyListSuite extends FunSuite {
  test("flatMap_1") {
    val expected = MyList()
    val actual = flatMap(MyList(), MyList(_))
    assertEquals(actual, expected)
  }
  test("flatMap_2") {
    val expected = MyList()
    val actual = flatMap(MyList(1, 2, 3), _ => MyList())
    assertEquals(actual, expected)
  }
  test("flatMap_3") {
    val expected = MyList(1, 2, 3)
    val actual = flatMap(MyList(1, 2, 3), MyList(_))
    assertEquals(actual, expected)
  }
  test("flatMap_4") {
    val expected = MyList(2, 4, 6)
    val actual = flatMap(MyList(1, 2, 3), x => MyList(2*x))
    assertEquals(actual, expected)
  }
  test("flatMap_5") {
    val expected = MyList(-11, -10, -9, -1, 0, 1, 9, 10, 11)
    val actual = flatMap(MyList(-10, 0, 10), x => MyList(x-1, x, x+1))
    assertEquals(actual, expected)
  }
  test("any_1") {
    val expected = false
    val actual = any(MyList(), _ => true)
    assertEquals(actual, expected)
  }
  test("any_2") {
    val expected = false
    val actual = any(MyList(1, 2, 3), _ => false)
    assertEquals(actual, expected)
  }
  test("any_3") {
    val expected = true
    val actual = any(MyList(1, 2, 3), _ == 2)
    assertEquals(actual, expected)
  }
  test("any_4") {
    val expected = false
    val actual = any(MyList(1, 2, 3), _ == 5)
    assertEquals(actual, expected)
  }
  test("any_5") {
    val expected = true
    val actual = any(MyList(1, 2, 3, 5, 6, 7), _ > 4)
    assertEquals(actual, expected)
  }
  test("all_1") {
    val expected = true
    val actual = all(MyList(), _ => false)
    assertEquals(actual, expected)
  }
  test("all_2") {
    val expected = true
    val actual = all(MyList(1, 2, 3), _ => true)
    assertEquals(actual, expected)
  }
  test("all_3") {
    val expected = false
    val actual = all(MyList(1, 2, 3), _ == 2)
    assertEquals(actual, expected)
  }
  test("all_4") {
    val expected = true
    val actual = all(MyList(1, 2, 3), _ != 5)
    assertEquals(actual, expected)
  }
  test("all_5") {
    val expected = false
    val actual = all(MyList(1, 2, 3, 5, 6, 7), _ > 4)
    assertEquals(actual, expected)
  }
  test("sumBy_1") {
    val expected = 0
    val actual = sumBy[Int](MyList(), x => x)
    assertEquals(actual, expected)
  }
  test("sumBy_2") {
    val expected = 0
    val actual = sumBy(MyList(1, 2, 3), _ => 0)
    assertEquals(actual, expected)
  }
  test("sumBy_3") {
    val expected = 6
    val actual = sumBy(MyList(1, 2, 3), x => x)
    assertEquals(actual, expected)
  }
  test("sumBy_4") {
    val expected = 12
    val actual = sumBy(MyList(1, 2, 3), x => 2*x)
    assertEquals(actual, expected)
  }
  test("sumBy_5") {
    val expected = 12
    val actual = sumBy(MyList(-3, -2, -1, 0, 1, 2, 3), x => math.abs(x))
    assertEquals(actual, expected)
  }
}
