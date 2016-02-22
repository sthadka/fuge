# fuge

fuge is an Erlang library for carefully refactoring critical paths. Fuge
allows you to implement [Branch by
Abstraction](http://martinfowler.com/bliki/BranchByAbstraction.html) in
production.

## Inspiration

See the excellent [GitHub Scientist
Post](http://githubengineering.com/scientist/) on why this pattern is useful.

[Github Scientist Ruby library](https://github.com/github/scientist)

## Features

Current set of features supported by fuge:

* Run old code alongside one or multiple versions of new code. fuge runs all
  the versions of the code provided and provides results in a stable order.
  Only the result of the control code is returned.
* Compare the results of each version. The output of each of version executed
  is available for comparison.
* Get run time for each version. Run time allows us to compare and see which
  version of the code is faster.
* The different code versions are run in a random order. This allows us to
  average out the advantage/disadvantage of the code's run order.
* Subscriber model for publishing information. fuge is agnostic to how results
  are used afterwards and is extensible using subscribers. fuge also provides
  a way to maintain state for each subscriber, removing the necessity of
  making the subscriber a process with state.
* Allows context information for every run. Context information can allow
  debugging the performance difference and it is passed on to the subscribers
  as is.
* Control the frequency of how often the experiment is run. e.g. run the
  experiment 42% of the time. This allows us to run the experiment less
  frequently in cases when the overhead of running multiple versions is large.

## Warning

Please keep the following in mind before using fuge in production!

* fuge is currently in alpha stage and in heavy development.
* fuge is to be run only on code without side effects and code that is
  idempotent.
* While fuge itself has a very low overhead (one ETS read, timing information,
  one gen_server cast), running multiple versions of the code in production
  will have a performance hit.

## Usage

We try to find the sum of consecutive numbers for illustration.

```erlang
% Start fuge application
1> ok = application:start(fuge).
ok
% Create a new fuge
2> ok = fuge:new(my_fuge).
ok
% Control code
3> Control = fun () -> lists:sum(lists:seq(1, 1000)) end.
#Fun<erl_eval.20.54118792>
% Candidate code
4> Candidate = fun() -> 1000 * (1000 + 1) div 2 end.
#Fun<erl_eval.20.54118792>
% Run the experiment
5> fuge:run(my_fuge, Control, Candidate).
500500
```

Output with the default logging subscriber:

```erlang
=INFO REPORT==== 18-Feb-2016::07:55:22 ===
Fuge: {fuge,my_fuge,[fuge_subscriber_logger],[]}
=INFO REPORT==== 18-Feb-2016::07:55:22 ===
Result: {fuge_result,undefined,
                     {fuge_data,81,500500},
                     [{fuge_data,52,500500}],
                     [0,1]}
```

In the "Result" portion, we can see that the first version of the code ran in
81 microseconds vs the candidate's 52 microseconds. We can also see that the
value "500500" returned by both the versions is the same.

## Planned Features

* Subscribers for different use cases (e.g. graphite)

## Roadmap

* Tests for edge cases
* Benchmark
* Run subscribers in separate process to avoid building a queue on fuge_server.
* Maybe create an example application for clear understanding of usage.
* Add options to fine tune the run.
