-module(ecp_tag_util).

-export([
  get_tag_ids/1, 
  pretty_print/1, 
  get/2,
  get_id/1, 
  get_type/1, 
  get_value/1,
  instances/1,
  instance/1,
  instance/2, 
  instance/3, 
  parse/1, 
  parse_tag_ids/1,
  make_tag_audit_log_tags/1, 
  has_tag/2, 
  parse_tags_string/1, 
  has_all_required_tags_for_message/1,
  ensure_has_tag/2,
  inject_tags/3,
  merge_message_type/2, merge_client/2, merge_client_string/2, merge_system/2, merge_sub_system/2, merge/3,
  create_id/2
]).

-include("include/ecp.hrl").

get(id, Tag) ->
  get_id(Tag);
  
get(type, Tag) ->
  get_type(Tag);

get(value, Tag) ->
  get_value(Tag).

get_id(#ecp_tag{id=Id}) ->
  Id.

get_type(#ecp_tag{type=Type}) ->
  Type.

get_value(#ecp_tag{value=Value}) ->
  Value.

instance(TagString) ->
  case parse(TagString) of
    {ok, Type, Value} ->
      instance(Type, Value);
    UNRECOGNIZED ->
      UNRECOGNIZED
  end.
  
instance(Type, Value) ->
  #ecp_tag{type=Type,value=Value, id=create_id(Type, Value)}.

instance(Type, Value, Id) ->
  #ecp_tag{type=Type,value=Value, id=Id}.
  
create_id(Type, Value) ->
  Type ++ ":" ++ Value.
  
instances('') ->
  [];

instances("") ->
  [];

instances(ShorthandStrings) ->
  instances(ShorthandStrings, []).

instances([H|T], Results) ->
  Tag = instance(H),
  instances(T, [Tag|Results] );

instances([], Results) ->
  Results.


get_tag_ids(TagRecords) ->
  get_tag_ids(TagRecords, []).

get_tag_ids([H|T], Results) ->
  get_tag_ids(T, [get_id(H)|Results]);
  
get_tag_ids([], Results) ->
  lists:reverse(Results).
  
pretty_print(TagRecord) ->
  lists:flatten(io_lib:format("~s:~s",[get_type(TagRecord), get_value(TagRecord)])).
  
parse({Type, Value}) ->
  {ok, Type, Value};
parse(ShorthandString) ->
  case re:split(ShorthandString,"[:]",[{return,list},{parts,2}]) of
    [Type, Value] ->
      {ok, Type, Value};
    _Unparseable ->
      {unrecognized_format, ShorthandString}
  end.
  
parse_tags_string(undefined) ->
  {ok, []};

parse_tags_string(String) ->
  parse_tags_string(string:tokens(String,","),[]).
  
parse_tags_string([Token|Tail],Result) ->
  case parse(string:strip(Token)) of
    {ok, Type, Value} ->
      parse_tags_string(Tail, [ proplists:property(Type,Value)|Result]);
    {unrecognized_format, ShorthandString} ->
      {unrecognized_format, ShorthandString}
  end;

parse_tags_string([],Result) ->
  {ok, Result}.

%
% parse a list of tag IDs into a list of ecp_tag records
%
% The function uses the parse function to parse out the list
% of tags.  If that function returns a problem then this function
% will throw unrecognized_format
%
parse_tag_ids(List) -> parse_tag_ids(List, []).
parse_tag_ids([], Result) -> Result;
parse_tag_ids([TagId | Tail], Result) ->
	case parse(TagId) of
		{ ok, Type, Value } ->
			Tag = #ecp_tag{ 
				id = Type ++ ":" ++ Value, 
				type = Type, 
				value = Value
			},
			Result2 = [ Tag | Result ],
			parse_tag_ids(Tail, Result2);

		{ unrecognized_format, _ } ->
			throw(unrecognized_format)
	end.


make_tag_audit_log_tags(TagRecords) ->
  make_tag_audit_log_tags(TagRecords, []).
make_tag_audit_log_tags([H|T], Results) ->
  make_tag_audit_log_tags(T, [{tag,pretty_print(H)}|Results]);
make_tag_audit_log_tags([], Results) ->
  lists:reverse(Results).
  
has_tag(Key, Tags) ->
  Predicate = fun(E) -> E#ecp_tag.type == Key end,
  SearchResults = lists:filter(Predicate, Tags),
  length(SearchResults) > 0.
  
ensure_has_tag(_TagRecords, []) ->
  ok;
ensure_has_tag(TagRecords, [Tag|Tags]) ->
  case has_tag(Tag, TagRecords) of
    true ->
      ensure_has_tag(TagRecords, Tags);
    false ->
      {missing, Tag}
  end.
  
has_all_required_tags_for_message(TagRecords) ->
  ensure_has_tag(TagRecords, [?CLIENT_TAG_TYPE, ?CLIENT_STRING_TAG_TYPE, ?MESSAGE_TYPE_TAG_TYPE, ?SYSTEM_TAG_TYPE, ?SUB_SYSTEM_TAG_TYPE]).

  
inject_tag([TagId|TagIds],TagRecords, Overwrite) ->
  TmpTag = instance(TagId),
  ScrubbedTagRecords = case Overwrite of
    true ->
      Predicate = fun(E) -> E#ecp_tag.type /= TmpTag#ecp_tag.type end,
      lists:filter(Predicate, TagRecords);
    false ->
      TagRecords
  end,
  inject_tag(TagIds, [TmpTag|ScrubbedTagRecords], Overwrite);

inject_tag([], TagRecords, _Overwrite) ->
  TagRecords.


inject_tags([], TagRecords, _Overwrite) ->
  TagRecords;
  
inject_tags(undefined, TagRecords, _Overwrite) ->
  TagRecords;
  
inject_tags(TagIds, TagRecords, Overwrite) ->
  inject_tag(TagIds, TagRecords, Overwrite).
  
merge_message_type(Tags, Value) ->
  ecp_tag_util:merge(Tags, "MessageType", Value).
merge_client(Tags, Value) ->
  ecp_tag_util:merge(Tags, "Client", Value).
merge_client_string(Tags, Value) ->
  ecp_tag_util:merge(Tags, "ClientString", Value).
merge_system(Tags, Value) ->
  ecp_tag_util:merge(Tags, "System", Value).
merge_sub_system(Tags, Value) ->
  ecp_tag_util:merge(Tags, "SubSystem", Value).
  
merge(Tags, _Key, "") ->
  Tags;
merge(Tags, _Key, undefined) ->
  Tags;
merge(Tags, Key, Value) ->
  proplists:delete(Key,Tags) ++ [proplists:property(Key,Value)].


