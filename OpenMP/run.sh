#!/bin/bash

# DPT assessed exercise stage 1 runner script - C+OpenMP
# Initial version by Gabor Borics-Kurti
#
# To run all C+OpenMP measurements, simply run this file.

set -e;

# OpenMP might not be found on macOS, so look for the latest
# GCC version installed with Homebrew (brew install gcc).
# Also work around the lack of GNU time on macOS by using
# the gnu-time formula from Homebrew (brew install gnu-time).
if [[ "$OSTYPE" == "darwin"* ]]; then
  COMPILER="/usr/local/Cellar/gcc/9.2.0_1/bin/gcc-9";
  TIMER="gtime";
else
  COMPILER="gcc";
  TIMER="time";
fi

FILENAME="results.csv";
UPPER_LIMITS=(15000 30000 60000);
DATASET_IDS=(0 1 2);
SEQUENTIAL="totientRange";
# Assumes that the parallel implementation is saved in totientRangePar.c
PARALLEL="totientRangeParallel";

# Runs and times a sequential totient calculation between 1
# and the first argument. Writes the results to FILENAME
function runSequentialCalc {
  RESULT=$( { ${TIMER} -q -f %e ./${SEQUENTIAL} 1 "$1" ; } 2>&1 );

  OUTPUT=();
  while read -r line; do OUTPUT+=("${line}"); done <<< "${RESULT}";

  echo "Sequential runtime for ${DATASET_NAME}: ${OUTPUT[1]}";
  printf "%s,sequential,%s,%s\n" "${DATASET_NAME}" "${OUTPUT[1]}" "${OUTPUT[0]}" >> ${FILENAME};

  return 0;
}

# Runs and times a parallel totient calculation between 1
# and the first argument. Writes the results to FILENAME
function runParallelCalc {
  RESULT=$( { ${TIMER} -q -f %e ./${PARALLEL} 1 "$1" "$2" ; } 2>&1 );

  OUTPUT=();
  while read -r line; do OUTPUT+=("${line}"); done <<< "${RESULT}";

  echo "Parallel runtime for ${DATASET_NAME} with $2 threads: ${OUTPUT[1]}";
  printf "%s,%s,%s,%s\n" "${DATASET_NAME}" "$2" "${OUTPUT[1]}" "${OUTPUT[0]}" >> ${FILENAME};

  return 0;
}

# Compile both sequential and parallel versions
${COMPILER} -Wall -O2 -o ${SEQUENTIAL} ${SEQUENTIAL}.c;
${COMPILER} -Wall -fopenmp -O2 -o ${PARALLEL} ${PARALLEL}.c;

# Write CSV headers
printf "dataset,threads,time,output\n" > ${FILENAME};

# Run each measurement three times
for INDEX in 0 1 2
do
  UPPER_LIMIT=${UPPER_LIMITS[$INDEX]};
  DATASET_NAME="DS$((INDEX+1))";

  if [[ "(0 1)" =~ ${INDEX} ]]; then
    echo "Running both sequential and parallel versions for ${DATASET_NAME}";
    for _ in "${DATASET_IDS[@]}"
    do
      runSequentialCalc "$UPPER_LIMIT";
    done
    for THREADS in 1 2 4 8 12 16 24 32 48 64
    do
      echo "Running ${DATASET_NAME} with ${THREADS} threads";
      for _ in "${DATASET_IDS[@]}"
      do
        runParallelCalc "$UPPER_LIMIT" $THREADS;
      done
    done
  else
    echo "Skipping sequential version for ${DATASET_NAME}";
    for THREADS in 8 16 32 64
    do
      echo "Running ${DATASET_NAME} with ${THREADS} threads";
      for _ in "${DATASET_IDS[@]}"
      do
        runParallelCalc "$UPPER_LIMIT" $THREADS;
      done
    done
  fi
done

exit 0;