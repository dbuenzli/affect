This project uses [`b0`] for development. Consult [b0 occasionally]
for quick hints on how to test and run builds.

[`b0`]: https://erratique.ch/software/b0
[b0 occasionally]: https://erratique.ch/software/b0/doc/occasionally.html

# Testing

If you witness flaky test failures it's likely a race condition bug.
The test excutables have a `--repeat/-R` option that allows to repeat
the tests the number of specified times which allows to witness them
more and `--fail-stop/-S` to stop when it fails. 

In our experience scheduler bug tend to show up with at least with `-R
10000` and more often with on a busy system (e.g. if you are running
another OS with `qemu`).

The option `--parallel-tracing/-T` option can be used to enable affect
tracing, invoke the test executables with `--help` for more options.

```
b0 list --tests
b0 test
REPEAT=1000 b0 test
b0 -- test_affect -R 1000 -T -S  # Repeat 1000, trace and stop on failure
b0 -- test_affect -T -F          # Trace only function activity 
b0 -- test_affect -T -F |& less -R
b0 -- test_effect --help
```

# Samples 

Treat yourself to a fractal rendering:

```
b0 -- mandelbrot /tmp/mandelbrot.img
```

Or hypnotize yourself by running in separate terminals

```
b0 -- ping
```

# Benchmarking 

A couple of benchmark are in the repo. You'll need the `hyperfine`
tool. They write their results in [`test/bench`]. For example:

```
b0 list --benchs
b0 -- bench-manderbrot
```

The JSON results can be visualized on [this page](https://try.venz.dev/).
A few SVG of test runs are in [`test/bench`].

[`test/bench`]: test/bench
