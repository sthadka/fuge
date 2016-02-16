-record(fuge_subscriber, {
          % Name of the subscriber
          name,
          % State of the subscriber. e.g. Connection information to third-party
          state
         }).

-record(fuge, {
          % Name of the experiment
          name :: string() | binary(),
          % List of middlewares to be run for this experiment
          subscribers :: [#fuge_subscriber{}],
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
