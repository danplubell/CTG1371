module CTG1371Bench (benchmarks) where

import CTG1371

import Criterion

benchmarks :: [Benchmark]
benchmarks =
    [ bench "main" (nfIO main)
    ]
