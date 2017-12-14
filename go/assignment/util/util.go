/// ----------------------------------------------------------------------------
/// @author Duncan Paul Attard
/// @author Kevin Vella
///
/// Exports a number of utility functions.
/// ----------------------------------------------------------------------------
package util

import(
  "testing"
  "fmt"
  "sync"
)

// -----------------------------------------------------------------------------
// Determines whether the specified arguments expected and actual are equal, and
// if not, outputs msg.
// func AssertEqual(t, a interface{}, b interface{}, msg) where:
//   * t is the test state handle.
//   * expected is the expected value.
//   * actual is the actual value.
//   * msg is the error message to output in case expected != actual.
// Returns nothing.
// -----------------------------------------------------------------------------
func AssertEqual(t *testing.T, expected interface{}, actual interface{}, msg string) {
  if expected == actual {
    return
	}
	if len(msg) == 0 {
		msg = fmt.Sprintf("%v != %v", expected, actual)
	}
	t.Fatal(msg)
}

// -----------------------------------------------------------------------------
// Determines whether the specified arguments expected and actual are *not*
// equal, and if yes, outputs msg.
// func AssertNotEqual(t, expected, actual, msg) where:
//   * t is the test state handle.
//   * expected is the expected value.
//   * actual is the actual value.
//   * msg is the error message to output in case expected == actual.
// Returns nothing.
// -----------------------------------------------------------------------------
func AssertNotEqual(t *testing.T, expected interface{}, actual interface{}, msg string) {
  if (expected != actual) {
    return
  }
  if len(msg) == 0 {
    msg = fmt.Sprintf("%v != %v", expected, actual)
  }
  t.Fatal(msg)
}

// -----------------------------------------------------------------------------
// Provides PAR-like sugaring by creating a goroutine for each of the processes
// specified in procs, and then blocking the caller indefinitely until these
// processes terminate (either normally or abnormally).
// func Par(procs) where:
//   * procs is the list of processes to create.
// Returns nothing.
//
// This concurreny abstraction patters in also known as fork-join, where each
// of proc in procs is created in its own process, and then the parent process
// blocks and waits for all its children to complete their execution. In this
// implementation, we favour the use of sync.WorkGroup to achieve this effect.
// -----------------------------------------------------------------------------
func Par(procs ...func()) {
	var wg sync.WaitGroup // or just WaitGroup

  // Add the number of processes to wait for.
  wg.Add(len(procs))

  // Wait for the processes before the function exits.
	defer wg.Wait()

  // Create goroutine for each of proc in procs and invoke it. The func signals
  // 'done' to the main process once completed.
	for _, proc := range procs {
		go func(proc func()) {
			defer wg.Done()
			proc()
		}(proc)
	}
}
