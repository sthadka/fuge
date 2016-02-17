-record(fuge, {
          % Name of the experiment
          name :: string() | binary(),
          % List of middlewares to be run for this experiment
          subscribers :: list(fuge_subscriber()),
          % Options for the experiment
          options :: list()    % TODO
         }).

-record(fuge_data, {
          %% Time taken to run the code (in microseconds)
          duration :: integer(),
          %% Exceptions if any
          exception :: any(),
          %% Final value of the code
          value :: any()
         }).

-record(fuge_result, {
          % Meta information about the experiment for context (optional)
          context :: term(),
          % Data about control
          control :: #fuge_data{},
          % Data about candidate
          candidate :: #fuge_data{},
          % Order of execution
          execution_order :: list()
         }).

-type fuge_subscriber() :: atom().
-type fuge() :: #fuge{}.
-type result() :: #fuge_result{}.
-type subscriber_state() :: any().
-type name() :: term().
-type error() :: {error, atom()}.
