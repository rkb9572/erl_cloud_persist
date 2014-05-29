-module(ecp_sub).

-export([get/2, is/2, instance/9, instance/8, make_duplication_key/4, to_string/1]).

-include("include/ecp.hrl").

get(id, Subscription) ->
  Subscription#ecp_sub.id;
  
get(principal_id, Subscription) ->
  Subscription#ecp_sub.principal_id;

get(callback_url, Subscription) ->
  Subscription#ecp_sub.callback_url;

get(wsdl_uri, Subscription) ->
  Subscription#ecp_sub.wsdl_uri;

get(queue_name, Subscription) ->
  Subscription#ecp_sub.queue_name;

get(tags, Subscription) ->
  get_tags(get(tag_ids, Subscription));

get(tag_ids, Subscription) ->
  Subscription#ecp_sub.tag_ids;

get(date_created, Subscription) ->
  Subscription#ecp_sub.date_created;

get(date_cancelled, Subscription) ->
  Subscription#ecp_sub.date_cancelled;
  
get(delivery_type, Subscription) ->
  case get(callback_url, Subscription) of
    undefined ->
      case get(wsdl_uri, Subscription) of
        undefined ->
          none;
        _DEFINED1 ->
          wsdl
      end;
    _DEFINED2 ->
      callback
  end;
  
get(duplication_key, Subscription) ->
  Subscription#ecp_sub.duplication_key.
  
make_duplication_key(Callback, WsdlUri, PrincipalId, TagIds) ->
  ecp_util:undefined_2_empty(PrincipalId) ++ 
  ecp_util:undefined_2_empty(Callback) ++ 
  ecp_util:undefined_2_empty(WsdlUri) ++ 
  make_tags_string(TagIds).

make_tags_string(TagIds) ->
  lists:flatten(lists:sort(TagIds)).

is(cancelled, Sub) ->
  case get(date_cancelled, Sub) of
    undefined ->
      false;
    _DEFINED2 ->
      true
  end;
  
is(has_callback, Sub) ->
  case get(callback_url, Sub) of
    undefined ->
      case get(wsdl_uri, Sub) of
        undefined ->
          false;
        _DEFINED2 ->
          true
      end;
    _DEFINED1 ->
      true
  end;
  
is(watchable, Sub) ->
  case is(has_callback, Sub) of
    true ->
      case is(cancelled, Sub) of
        false ->
          {true};
        true ->
          {false, cancelled}
      end;
    false ->
      {false, no_callback}
  end.


instance(Id, PrincipalId, CallbackUrl, WsdlUri, QueueName, TagIds, DateCreated, DuplicationKey) ->
  instance(Id, PrincipalId, CallbackUrl, WsdlUri, QueueName, TagIds, DateCreated, undefined, DuplicationKey).
  
instance(Id, PrincipalId, CallbackUrl, WsdlUri, QueueName, TagIds, DateCreated, DateCancelled, DuplicationKey) ->
  #ecp_sub{
    id=Id, 
    principal_id=PrincipalId, 
    callback_url=CallbackUrl,
    wsdl_uri=WsdlUri,
    queue_name=QueueName,
    tag_ids=TagIds,
    date_created=DateCreated,
    date_cancelled=DateCancelled,
    duplication_key=DuplicationKey
  }.
  
get_tags(Ids) ->
  Tmp = pe_tag_util:instances(lists:sort(Ids)),
    
  Fun2 = fun(Elem) ->
    Elem == null
  end,
  
  lists:dropwhile(Fun2, Tmp).

%
% convert a subscription into a one-line string
%
to_string (Subscription) ->
	TagString = to_tag_string(Subscription#ecp_sub.tag_ids),
	io_lib:format(
		"{ ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p }", 
		[ 
			Subscription#ecp_sub.id,
			Subscription#ecp_sub.principal_id,
			Subscription#ecp_sub.callback_url,
			Subscription#ecp_sub.wsdl_uri,
			Subscription#ecp_sub.queue_name,
			TagString,
			Subscription#ecp_sub.date_created,
			Subscription#ecp_sub.date_cancelled,
			Subscription#ecp_sub.duplication_key
		]
	).

%
% convert a list of tags into a one-line string
%
to_tag_string ([]) -> "";
to_tag_string ([Head | Tail]) ->
	Head ++ ", " ++ to_tag_string(Tail).

