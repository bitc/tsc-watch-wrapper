# tsc-watch-wrapper

This program will run the TypeScript compiler in \"watch\" mode (in the current
directory). When any file changes, a new compilation will automatically be
triggered: when this compilation completes, the callback program that you
supplied will be run.

