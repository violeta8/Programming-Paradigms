/// ----------------------------------------------------------------------------
/// @author Duncan Paul Attard
///
/// Model answers for worksheet 1 questions.
/// ----------------------------------------------------------------------------
package util

// -----------------------------------------------------------------------------
// Computes factorial numbers using direct recursion.
// func Fact(n) where:
//   * n is the number whose factorial is to be computed.
// Returns: The factorial for index n.
// -----------------------------------------------------------------------------
func Fact(n int) int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Computes factorial numbers using indirect recursion.
// func Fact2(n) where:
//   * n is the number whose factorial is to be computed.
// Returns: The factorial for index n.
// -----------------------------------------------------------------------------
func Fact2(n int) int {
  return Fact3(n, 1)
}

func Fact3(n int, acc int) int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Computes fibonacci numbers using direct recursion.
// func Fib(n) where:
//   * n is the number whose fibonacci number is to be computed.
// Returns: The fibonacci number for index n.
// -----------------------------------------------------------------------------
func Fib(n int) int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Computes fibonacci numbers using indirect recursion.
// func Fib2(n) where:
//   * n is the number whose fibonacci number is to be computed.
// Returns: The fibonacci number for index n.
// -----------------------------------------------------------------------------
func Fib2(n int) int {
  return Fib3(n, 0, 1)
}

func Fib3(n int, n_minus2 int, n_minus1 int) int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Maps an array of elements into another array of elements having equal length.
// func Map(f, elems) where:
//   * f is a function that maps the current element x in the array to a new
//     value.
//   * elems is the array to map.
// Returns: The same array with f applied on each of its elements.
// -----------------------------------------------------------------------------
func Map(f func(int) int, elems []int) []int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Filters an array of elements according to the specifed predicate f.
// func Filter(f, elems) where:
//   * f is a function that tests the current element x in the array and returns
//     a boolean, indicating whether x is to be retained.
//   * elems is the array to filter.
// Returns: A new array filtered according to f, possibly containing fewer
//          elements than the original array.
// -----------------------------------------------------------------------------
func Filter(f func(int) bool, elems []int) []int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Reduces an array of elements using the specified combining function f,
// starting with the seed value.
// func Reduce(f, seed, elems) where:
//   * f is a function that combines the current element x in the array with the
//      next element x + 1 in the array.
//   * seed is the seed that the reduction starts with.
//   * elems is the array to reduce.
// Returns: A single value that reflects the reduction of elems.
// -----------------------------------------------------------------------------
func Reduce(f func(x int, acc int) int, seed int, elems []int) int {
  // TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Reverses the specified list.
// func Reverse(elems) where:
//   * elems is the array to be reversed.
// Returns: A new array that is the reverse of elems.
// -----------------------------------------------------------------------------
func Reverse(elems []int) []int {
  // TODO: Add implementation.
}
