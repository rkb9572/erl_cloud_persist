-module(ecp_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Key, Value) ->
  supervisor:start_child(?SERVER, [Key, Value]).

start_child(Spec) ->
  supervisor:start_child(?SERVER, Spec).  
  
init([]) ->
  	SubStore = ?CHILD(ecp_sub_store, worker),  	
  	PrincipalStore = ?CHILD(ecp_principal_store, worker),
  	SubIntake = ?CHILD(ecp_sub_intake, worker),  	
  	PrincipalIntake = ?CHILD(ecp_principal_intake, worker),
	
	%%
	%% The 0,1 restart intensity will cause this supervisor to exit if any of
	%% its children fail.  This is needed because of dependencies on these 
	%% children from children under other supervisors.  The parent of this 
	%% supervisor will do the right thing as long as this supervisor exits when
	%% any of its children fail.  Given this, the rest_for_one restart strategy
	%% is a bit academic.  That said, due to startup and runtime dependencies
	%% between the children, it is correct to start any gen_servers dependent
	%% on a failed gen_server.
	%%
  	{ok, {{rest_for_one, 0,1}, [SubStore, PrincipalStore, SubIntake, PrincipalIntake]}}.

