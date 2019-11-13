// TotientRangePar.c - Parallel Euler Totient Function (C Version)
// compile: gcc -Wall -O -o TotientRangePar TotientRangePar.c
// run:     ./TotientRangePar lower_num upper_num num_threads(optional)

// Author: Max Kirker Burton 2260452b     13/11/19

// This program calculates the sum of the totients between a lower and an 
// upper limit using C longs, and can be run with several Goroutines either set as an argument or
// as an environment variable
// It is based on earlier work by:
// Phil Trinder, Nathan Charles, Hans-Wolfgang Loidl and Colin Runciman

// The comments provide (executable) Haskell specifications of the functions

#include <stdio.h>
#include <omp.h>
#include <sys/time.h>

// hcf x 0 = x
// hcf x y = hcf y (rem x y)

long hcf(long x, long y)
{
  long t;

  while (y != 0) {
    t = x % y;
    x = y;
    y = t;
  }
  return x;
}


// relprime x y = hcf x y == 1

int relprime(long x, long y)
{
  return hcf(x, y) == 1;
}


// euler n = length (filter (relprime n) [1 .. n-1])

long euler(long n)
{
  long length, i;

  length = 0;
  for (i = 1; i < n; i++)
    if (relprime(n, i))
      length += 1;
  return length;
}


// sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

long sumTotient(long lower, long upper, int n_threads)
{
  long sum, i;

  sum = 0;
  #pragma omp parallel for schedule(guided) reduction(+: sum) num_threads(n_threads)
    for (i = lower; i <= upper; i++)
      sum += euler(i);
  return sum;
}


int main(int argc, char ** argv)
{
  long lower, upper;
  int num_threads = omp_get_num_threads();
  float msec;
  struct timeval start, stop;

  if (argc < 3) {
    printf("fewer than 2 arguments\n");
    return 1;
  }
  sscanf(argv[1], "%ld", &lower);
  sscanf(argv[2], "%ld", &upper);
  if (argc == 4){
    sscanf(argv[3], "%d", &num_threads);
  }

  gettimeofday(&start, NULL);	
  printf("C: Sum of Totients  between [%ld..%ld] is %ld\n",
         lower, upper, sumTotient(lower, upper, num_threads));
  gettimeofday(&stop, NULL);
  if (stop.tv_usec < start.tv_usec) {
    stop.tv_usec += 1000000;
    stop.tv_sec--;
  }
  msec = 1000 * (stop.tv_sec - start.tv_sec) +
                (stop.tv_usec - start.tv_usec) / 1000;

  printf("%f\n", msec);  // Rename to elapsed time:
  return 0;
}