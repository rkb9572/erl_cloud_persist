-module(ecp_principal_intake).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([
	accept/7, 
	activate/1, 
	deactivate/1, 
	start_link/0
]).

-include("include/ecp.hrl").
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask) ->
  gen_server:call(?MODULE, {new, PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask}).
  
deactivate({id, PrincipalId}) ->
  {found, Principal} = ecp_principal_store:lookup(PrincipalId),
  gen_server:call(?MODULE, {deactivate, Principal});

deactivate({ecp_principal, Principal}) ->
  gen_server:call(?MODULE, {deactivate, Principal});

deactivate(Principal) ->
  deactivate({ecp_principal, Principal}).

activate({id, PrincipalId}) ->
  {found, Principal} = ecp_principal_store:lookup(PrincipalId),
  gen_server:call(?MODULE, {activate, Principal});

activate({ecp_principal, Principal}) ->
  gen_server:call(?MODULE, {activate, Principal});

activate(Principal) ->
  activate({ecp_principal, Principal}).



init([]) ->
  {ok, []}.
  

create_secret_or_use_provided(undefined) ->
  ecp_util:generate_key();
  
create_secret_or_use_provided(Secret) ->
  Secret.
  
principal_exists(PrincipalId) ->
  case ecp_principal_store:lookup(PrincipalId) of
    none ->
      false;
    {found, _Found} ->
      true
  end.

handle_call({new, PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask}, _From, _State) ->
  TagRecords = ecp_tag_util:instances(EnforcedTags),

  AssignedSecret = create_secret_or_use_provided(Secret),
  
  Result = case principal_exists(PrincipalId) of
    false ->
      {created, Principal} = ecp_principal_store:create_new(
        ecp_principal:instance(PrincipalId, FriendlyName, ecp_tag_util:get_tag_ids(TagRecords), RequireMessageTypeWithSubscription, AssignedSecret, undefined, Realm, DeliveryUrlMask)
      ),
  
      Tmp2 = ecp_tag_util:make_tag_audit_log_tags(TagRecords),
      Tmp3 = [{principal,PrincipalId},{friendly_name, FriendlyName},{is_require_message_type_with_new_subs, RequireMessageTypeWithSubscription},{realm, Realm}],
      
      pe_audit:log(lists:append(Tmp3,Tmp2),"PRINCIPAL"),

      event_manager:notify({principal_created, Principal}),
      {ok,Principal};
    true ->
      {error, duplicate_principal_id}
  end,
  {reply,Result,_State};
  
handle_call({activate, Principal}, _From, _State) ->
  {cancelled,_Principal} = ecp_principal_store:activate(Principal),
  {reply,{ok,_Principal},_State};

handle_call({deactivate, Principal}, _From, _State) ->
  {cancelled,_Principal} = ecp_principal_store:deactivate(Principal),
  {reply,{ok,_Principal},_State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
