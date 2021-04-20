package lego

import (
	"sync"
	"../util"
)


// -----------------------------------------------------------------------------
// Implements a 1-place buffer, allowing the in and out channels to be decoupled
// by a single message.
// func Id(in, out chan) where:
//   * in is the input channel.
//   * out is the output channel.
// -----------------------------------------------------------------------------
func Id(in, out chan int) {
	for {
		i := <-in
		out <- i
	}
}

// -----------------------------------------------------------------------------
// Computes the successor of an integer.
// func Succ(in, out chan) where:
//   * in is the input channel.
//   * out is the output channel producing in + 1.
// -----------------------------------------------------------------------------
func Succ(in, out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Eats up the value that is presented at the input channel.
// func Sink(in chan) where:
//   * in is the sink channel.
// -----------------------------------------------------------------------------
func Sink(in chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Removes the first input on the channel and allowing the rest (the tail) to
// pass through.
// func Tail(in, out chan) where:
//   * in is the input channel.
//   * out is the output channel to which the input from in is output, less
//     the first input that was read.
// -----------------------------------------------------------------------------
func Tail(in, out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Prepends the integer n to the head of the output the channel.
// func Prefix(n, in, out chan) where:
//   * n is the number to produce on the out channel first.
//   * in is the input channel.
//   * out is the output channel to which the prefix n and the rest of the
//     input from in is output.
// -----------------------------------------------------------------------------
func Prefix(n int, in, out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Adds two numbers presented at the input channels in the *predetermined* order
// of in_x followed by in_y.
// func PlusSerial(in_x, in_y, out chan) where:
//   * in_x is the first number to be added.
//   * in_y is the second number to be added.
//   * out produces the addition of the numbers presented on in_x and in_y.
// -----------------------------------------------------------------------------
func PlusSerial(in_x, in_y, out chan int) {
	for {
		x := <- in_x
		y := <- in_y
		out <- x + y
	}
}

// -----------------------------------------------------------------------------
// Adds two numbers presented at the input channels, the first to be read either
// at in_x or at in_y. This is a wrong implementation of Plus because in certain
// cases we can add two successive numbers from in_x or two successive numbers
// from in_y, whereas our intended behaviour is that of adding one number from
// in_x and the other from in_y together.
// Note that the variables x and y are both initialized to 0 before we start 
// reading from in_x and in_y respectively.
// func PlusWrong(in_x, in_y, out chan) where:
//   * in_x is a number to be added.
//   * in_y is a number to be added.
//   * out produces the addition of numbers in the following possibilities:
//     1. in_x + 0 (the 1st read from in_x is discarded when the 2nd read from 
//                  in_x performed; variable y = 0)
//     2. in_y + 0 (the 1st read from in_y is discarded when the 2nd read from 
//                  in_y performed; variable x = 0)
//     3. in_x + in_y
//     4. in_y + in_x
// -----------------------------------------------------------------------------
func PlusWrong(in_x, in_y, out chan int) {
	for {
		x, y := 0, 0
		for i:= 0; i < 2; i++ {
			select {
				case x = <- in_x:
				case y = <- in_y:
			}
		}
		out <- x + y
	}
}

// -----------------------------------------------------------------------------
// Adds two numbers presented at the input channels in *any* order.
// func PlusNaive(in_x, in_y, out chan) where:
//   * in_x is the first number to be added.
//   * in_y is the second number to be added.
//   * out produces the addition of the numbers presented on in_x and in_y.
//
// This implementation, although a corret one as it works as intended by either
// adding in_x + in_y or in_y + in_x, is naive, because as seen in the select
// statement, we are enumerating all the possible interleavings in which inputs
// can appear on the input channels in_x and in_y:
//   1. in_x can be read first, followed by in_y
//   2. in_y can be read first, followed by in_x.
// The number of possible permutations (i.e. not combinations, since order
// matters) in which inputs can be interleaved grows according to the number of
// input channels. This number is substantial and can quickly lead to
// unmanageable code!
// -----------------------------------------------------------------------------
func PlusNaive(in_x, in_y, out chan int) {
	for {
		x, y := 0, 0
		select {
		case x = <- in_x:
			y = <- in_y
		case y = <- in_y:
			x = <- in_x
		}
		out <- x + y
	}
}

// -----------------------------------------------------------------------------
// Adds two numbers presented at the input channels in *any* order.
// func PlusWG(in_x, in_y, out chan) where:
//   * in_x is the first number to be added.
//   * in_y is the second number to be added.
//   * out produces the addition of the numbers presented on in_x and in_y.
//
// This implementation employs the sync.WaitGroup.
// -----------------------------------------------------------------------------
func PlusWG(in_x, in_y, out chan int) {
	var wg sync.WaitGroup
	for {
		x, y := 0, 0
		wg.Add(2)
		go func() {
			defer wg.Done()
			x = <- in_x
		}()
		go func() {
			defer wg.Done()
			y = <- in_y
		}()
		wg.Wait()
		out <- x + y
	}
}

// -----------------------------------------------------------------------------
// Adds two numbers presented at the input channels in *any* order.
// func Plus(in_x, in_y, out chan) where:
//   * in_x is the first number to be added.
//   * in_y is the second number to be added.
//   * out produces the addition of the numbers presented on in_x and in_y.
//
// This implementation employs the Par (or fork-join) concurreny abstraction
// pattern, and works exactly like the one using sync.WaitGroup.
// -----------------------------------------------------------------------------
func Plus(in_x, in_y, out chan int) {
	for {
		x, y := 0, 0
		util.Par(func() {
			x = <- in_x
		}, func() {
			y = <- in_y
		})
		out <- x + y
	}
}

// -----------------------------------------------------------------------------
// Replicates the number presented at the input channel to out_x and out_y.
// func Delta(in, out_x, out_y chan) where:
//   * in is the number to be replicated.
//   * out_x produces a copy of the number presented at in.
//   * out_x produces a copy of the number presented at in.
//
// This implementation employs the sync.WaitGroup.
// -----------------------------------------------------------------------------
func DeltaWG(in, out_x, out_y chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Replicates the number presented at the input channel to out_x and out_y.
// func Delta(in, out_x, out_y chan) where:
//   * in is the number to be replicated.
//   * out_x produces a copy of the number presented at in.
//   * out_x produces a copy of the number presented at in.
//
// This implementation employs the Par (or fork-join) concurreny abstraction
// pattern, and works exactly like the one using sync.WaitGroup.
// -----------------------------------------------------------------------------
func Delta(in, out_x, out_y chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Produces the list of integers greater than zero by outputting the next
// number each time this is read from the output channel.
// func Nos(out chan) where:
//   * out is the output channel where the numbers are output.
// -----------------------------------------------------------------------------
func Nos(out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Produces the running total of the numbers presented in the input channel in
// (an integrator).
// func Int(in, out chan) where:
//   * in is the input channel where the numbers to be added are read.
//   * out produces the running total of the numbers input till now.
// -----------------------------------------------------------------------------
func Int(in, out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Outputs the numbers that are seperated by a difference of two.
// func Pairs(in, out chan) where:
//   * in is the input channel where the numbers to be paired are presented.
//   * out produces the pairs of numbers.
// -----------------------------------------------------------------------------
func Pairs(in, out chan int)  {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Produces the list of numbers from the fibonacci sequence starting from 0.
// func Fib(out chan) where:
//   * out is the output channel where the numbers are output.
// -----------------------------------------------------------------------------
func Fib(out chan int) {
	// TODO: Add implementation.
}

// -----------------------------------------------------------------------------
// Produces the list of square numbers starting from 1.
// func Squares(out chan) where:
//   * out is the output channel where the square numbers are output.
// -----------------------------------------------------------------------------
func Squares(out chan int) {
	// TODO: Add implementation.
}
