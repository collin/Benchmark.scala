package com.balagez.example

import com.balagez.testing.Benchmark

/**
 * Benchmarking examples.
 * 
 * Fibonacci examples taken from, see [[http://en.literateprograms.org/Fibonacci_numbers_(Scala)]]
 */
object BenchmarkExample {
	
	def main(argv: Array[String]) {
		
		// Simple measurement
		println("Benchmarking the speed of creating a 10M long string of \"a\"")
		val measurement = Benchmark.measure { "a" * 10485760 }
		println(measurement)
		println(measurement.s)
		println(measurement.ms)
		
		// Benchmarking of a code block with simple report
		// aka. lambda function of signature: code: (A) => Unit
		Benchmark.bm { x =>
			x.caption("Calculating Fibonacci(500) using tail recursive algorithm:")
			x.report { FibonacciTailRecusive.fib(500) }
		}
		
		// Adding labels to each reported code blocks
		val n = 30
		Benchmark.bm(20, { x =>
			x.caption("Calculating Fibonacci(%d) using different algorithms:".format(n))
			x.report("Recursive:", { FibonacciRecursive.fib(n) })
			x.report("Iterative:", { FibonacciIterative.fib(n) })
			x.report("Tail recursive:", { FibonacciTailRecusive.fib(n) })
			x.report("Functional stream:", { FibonacciFunctionalStream.fib(n) })
			x.report("Fold left:", { FibonacciFoldLeft.fib(n) })
			x.separator
			x.total()
			x.avg()
		})
	}
	
	/**
	 * Recursive Fibonacci algorithm
	 */
	object FibonacciRecursive {
		
		def fib(n: Int): Int = n match {
			case 0 | 1 => n
			case _ => fib(n-1) + fib(n-2)
		}
	}
	
	/**
	 * Iterative Fibonacci algorithm
	 */
	object FibonacciIterative {
		
		def fib(n: Int) = {
			var a = 0
			var b = 1
			
		    def next(a: Int, b: Int) = Pair(b, a + b);
			
			var i = 0
			while (i < n) {
				val Pair(c, d) = next(a, b);
				a = c
				b = d
				i = i +1
			}
			a
		}
	}
	
	/**
	 * Tail recursive Fibonacci algorithm 
	 */
	object FibonacciTailRecusive {
		
		def fib(n:Int) = fib_tr(n, 1, 0)
		
		/**
		 * Recursion for tail recursive Fibonacci algorithm
		 */
		def fib_tr( n: Int, b: Int, a: Int): Int = n match {
			case 0 => a
			case _ => fib_tr(n-1, a + b, b)
		}
	}
	
	/**
	 * Functional stream Fibonacci algorithm
	 */
	object FibonacciFunctionalStream {
		
		lazy val fib = {
			def f(a:Int, b:Int): Stream[Int] = a #:: f(b, a+b)
			f(0, 1)
		}
	}
	
	/**
	 * Fold left Fibonacci algorithm
	 */
	object FibonacciFoldLeft {
		
		def fib(n: Int) = ((0, 1) /: (1 to n)) ((a, dummy) => (a._2, a._1 + a._2))
	}
	
}