# tsc-watch-wrapper

This program will run the TypeScript compiler in "watch" mode (in the current
directory). When any file changes, a new compilation will automatically be
triggered: when this compilation completes, the callback program that you
supplied will be run.

## Usage

```
tsc-watch-wrapper - Enhanced TypeScript tsc "watch" functionality

Usage: tsc-watch-wrapper [--with-tsc PROG] CALLBACK_PROG [CALLBACK_ARGS...]

  CALLBACK_PROG            Program that will be called every time compilation
                           completes
  CALLBACK_ARGS            Optional command line arguments that will be passed
                           to the callback program

Available options:
  -h,--help                Show this help text
  --with-tsc PROG          Path to the tsc executable. Default is "tsc"

This program will run the TypeScript compiler in "watch" mode (in the current
directory). When any file changes, a new compilation will automatically be
triggered: when this compilation completes, the callback program that you
supplied will be run.
```
