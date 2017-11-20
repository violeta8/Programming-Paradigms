/// ----------------------------------------------------------------------------
/// @author Duncan Paul Attard
///
/// Provides a batch of unit tests for worksheet 1.
///
/// To run tests, use 'go test -v' on the command line from the package's
/// directory.
/// ----------------------------------------------------------------------------
package util

import (
  "testing"
  "fmt"
  "../util"
  )

// -----------------------------------------------------------------------------
// Tests the Fact functionality.
// -----------------------------------------------------------------------------
func TestFact(t *testing.T) {
  res := Fact(10)
  fmt.Println("Fact test:", res)
  util.AssertEqual(t, 3628800, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Fact2 functionality.
// -----------------------------------------------------------------------------
func TestFact2(t *testing.T) {
  res := Fact2(10)
  fmt.Println("Fact2 test:", res)
  util.AssertEqual(t, 3628800, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Fib functionality.
// -----------------------------------------------------------------------------
func TestFib(t *testing.T)  {
  res := Fib(10)
  fmt.Println("Fib test:", res)
  util.AssertEqual(t, 89, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Fib2 functionality.
// -----------------------------------------------------------------------------
func TestFib2(t *testing.T) {
  res := Fib(10)
  fmt.Println("Fib2 test:", res)
  util.AssertEqual(t, 89, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Map functionality.
// -----------------------------------------------------------------------------
func TestMapEmpty(t *testing.T) {
  ints := []int{}
  double := func(x int) int {
    return x * 2
  }

  res := Map(double, ints)
  fmt.Println("Map test with empty list:", res)
  util.AssertEqual(t, 0, len(res), "")
}

// -----------------------------------------------------------------------------
// Tests the Map functionality.
// -----------------------------------------------------------------------------
func TestMap(t *testing.T) {
  ints := []int{1, 2, 3, 4}
  double := func(x int) int {
    return x * 2
  }

  exp := []int{2, 4, 6, 8}
  res := Map(double, ints)
  fmt.Println("Map test with non-empty list:", res)
  for i, r := range res {
    util.AssertEqual(t, exp[i], r, "")
  }
}

// -----------------------------------------------------------------------------
// Tests the Filter functionality.
// -----------------------------------------------------------------------------
func TestFilterEmpty(t *testing.T) {
  ints := []int{}
  is_natural := func(x int) bool {
    return x >= 0
  }

  res := Filter(is_natural, ints)
  fmt.Println("Filter test with empty list:", res)
  util.AssertEqual(t, 0, len(res), "")
}

// -----------------------------------------------------------------------------
// Tests the Filter functionality.
// -----------------------------------------------------------------------------
func TestFilter(t *testing.T) {
  ints := []int{1, -2, 3, 4}
  is_natural := func(x int) bool {
    return x >= 0
  }

  exp := []int{1, 3, 4}
  res := Filter(is_natural, ints)
  fmt.Println("Filter test with non-empty list:", res)
  for i, r := range res {
    util.AssertEqual(t, exp[i], r, "")
  }
}

// -----------------------------------------------------------------------------
// Tests the Reduce functionality.
// -----------------------------------------------------------------------------
func TestReduceEmpty(t *testing.T) {
  ints := []int{}
  add := func(x int, acc int) int {
    return x + acc
  }

  res := Reduce(add, 0, ints)
  fmt.Println("Reduce test with empty list:", res)
  util.AssertEqual(t, 0, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Reduce functionality.
// -----------------------------------------------------------------------------
func TestReduce(t *testing.T) {
  ints := []int{1, 2, 3, 4}
  add := func(x int, acc int) int {
    return x + acc
  }

  res := Reduce(add, 0, ints)
  fmt.Println("Reduce test with non-empty list:", res)
  util.AssertEqual(t, 10, res, "")
}

// -----------------------------------------------------------------------------
// Tests the Reverse functionality.
// -----------------------------------------------------------------------------
func TestReverseEmpty(t *testing.T) {
  ints := []int{}

  res := Reverse(ints)
  fmt.Println("Reverse test with empty list:", res)
  util.AssertEqual(t, 0, len(res), "")
}

// -----------------------------------------------------------------------------
// Tests the Reverse functionality.
// -----------------------------------------------------------------------------
func TestReverseNonEmpty(t *testing.T) {
  ints := []int{1, 2, 3, 4}

  exp := []int{4, 3, 2, 1}
  res := Reverse(ints)
  fmt.Println("Reverse test with non-empty list:", res)
  for i, r := range res {
    util.AssertEqual(t, exp[i], r, "")
  }
}
