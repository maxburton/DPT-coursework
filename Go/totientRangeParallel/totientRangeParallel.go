// totientRangePar.go - Parallel Euler Totient Function (Go Version)
// compile: go build
// run:     totientRangePar lower_num upper_num num_threads

// Author: Max Kirker Burton 2260452b   07/11/2019

// This program calculates the sum of the totients between a lower and an
// upper limit, and can be run with several Goroutines
//
// Each function has an executable Haskell specification
//
// It is based on earlier work by: Greg Michaelson, Patrick Maier, Phil Trinder,
// Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

package main

import (
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"
)

// Compute the Highest Common Factor, hcf of two numbers x and y
//
// hcf x 0 = x
// hcf x y = hcf y (rem x y)

func hcf(x, y int64) int64 {
	var t int64
	for y != 0 {
		t = x % y
		x = y
		y = t
	}
	return x
}

// relprime determines whether two numbers x and y are relatively prime
//
// relprime x y = hcf x y == 1

func relprime(x, y int64) bool {
	return hcf(x, y) == 1
}

// euler(n) computes the Euler totient function, i.e. counts the number of
// positive integers up to n that are relatively prime to n
//
// euler n = length (filter (relprime n) [1 .. n-1])

func euler(lower int64, upper int64, ch chan int64, wg *sync.WaitGroup) {
	var length, i, j int64

	for i = lower; i < upper; i++ {
		length = 0
		for j = 1; j < i; j++ {
			if relprime(i, j) {
				length++
			}
		}
		ch <- length
	}
	wg.Done()
}

// sumTotient lower upper sums the Euler totient values for all numbers
// between "lower" and "upper".
//
// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

func sumTotient(lower, upper, cores int64) int64 {
	var sum, i int64
	ch := make(chan int64, 100000)
	sum = 0

	totalRange := upper - lower
	var goroutines int64 = cores
	chunkSize := totalRange / goroutines // floor division
	var wg sync.WaitGroup                // using a waitgroup to close the channel after all threads are completed

	for i = 0; i < goroutines; i++ {
		wg.Add(1)

		elower := lower + (chunkSize * i)

		var eupper int64
		if i+1 == goroutines {
			eupper = upper + 1 // plus 1 to include final number
		} else {
			eupper = lower + (chunkSize * (i + 1))
		}

		go euler(elower, eupper, ch, &wg) // round numbers, doesnt work perfectly as is on imperfect division
	}
	wg.Wait()
	close(ch)

	for value := range ch { // potentially change this so that summing happens as soon as one thread finishes
		sum += value
	}
	return sum
}

func main() {
	var lower, upper, cores int64
	var err error
	// Read and validate lower and upper arguments
	if len(os.Args) < 3 {
		panic(fmt.Sprintf("Usage: must provide lower and upper range limits as arguments"))
	}

	if lower, err = strconv.ParseInt(os.Args[1], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse first argument"))
	}
	if upper, err = strconv.ParseInt(os.Args[2], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse second argument"))
	}
	if cores, err = strconv.ParseInt(os.Args[3], 10, 64); err != nil {
		panic(fmt.Sprintf("Can't parse third argument"))
	}
	// Record start time
	start := time.Now()
	// Compute and output sum of totients
	fmt.Println("Sum of Totients between", lower, "and", upper, "is", sumTotient(lower, upper, cores))

	// Record the elapsed time
	t := time.Now()
	elapsed := t.Sub(start)
	fmt.Println("Elapsed time", elapsed)
}
