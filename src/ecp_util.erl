-module(ecp_util).

-export([
  to_binary/1, 
  create_signature/3, 
  verify_signature/4, 
  generate_key/0, 
  empty_2_undefined/1,
  undefined_2_empty/1,
  is_string_based_content_type/1,
  validate_payload_content_type/1,
  validate_input/2,
  validate_all_inputs/1
]).
 
empty_2_undefined("") ->
  undefined;

empty_2_undefined(undefined) ->
  undefined;
 
empty_2_undefined(Val) ->
  Val.
  
undefined_2_empty(undefined) ->
  "";
undefined_2_empty(Val) ->
  Val.
  
is_string_based_content_type("application/json") -> true;
is_string_based_content_type("application/xjson") -> true;
is_string_based_content_type("application/javascript") -> true;
is_string_based_content_type("application/x-javascript") -> true;
is_string_based_content_type("application/jsonp") -> true;
is_string_based_content_type("text/json") -> true;
is_string_based_content_type("text/javascript") -> true;
is_string_based_content_type("text/x-json") -> true;
is_string_based_content_type("text/x-javascript") -> true;
is_string_based_content_type("text/plain") -> true;
is_string_based_content_type("application/xml") -> true;
is_string_based_content_type("text/xml") -> true;
is_string_based_content_type("text/html") -> true;
is_string_based_content_type(_) -> false.
  


validate_payload_content_type("application/octet-stream") -> valid;
validate_payload_content_type("application/x-protobuf") -> valid;
validate_payload_content_type("application/json") -> valid;
validate_payload_content_type("application/x-json") -> valid;
validate_payload_content_type("text/json") -> valid;
validate_payload_content_type("text/x-json") -> valid;
validate_payload_content_type("text/javascript") -> valid;
validate_payload_content_type("text/x-javascript") -> valid;
validate_payload_content_type("application/x-javascript") -> valid;
validate_payload_content_type("application/javascript") -> valid;
validate_payload_content_type("text/plain") -> valid;
validate_payload_content_type("text/xml") -> valid;
validate_payload_content_type("application/xml") -> valid;
validate_payload_content_type("text/html") -> valid;
validate_payload_content_type("application/jsonp") -> valid;
validate_payload_content_type(_) -> invalid.
  
to_binary(undefined) ->
  to_binary("");
to_binary(String) when is_list(String) ->
  list_to_binary(String);
to_binary(Arg) when is_binary(Arg) ->
  Arg.

hex(Binary) ->
  hex:bin_to_hexstr(Binary).

generate_key() ->
  hex(crypto:rand_bytes(16)).
  
create_signature(Data, Key, Date) ->
  BinDate = to_binary(Date),
  BinData = to_binary(Data),
  WhatToSign = <<BinDate/binary,BinData/binary>>,
  %Digest = crypto:md5(WhatToSign),
  %Digest = sha2:hexdigest256(WhatToSign),
  %IV = <<0:128>>,
  %Signed = crypto:aes_cbc_128_encrypt(Key, IV, Digest),
  %Signed = cmac_aes_cbc_128(Key, WhatToSign),
  Signed = omac1:generate_tag_aes_cbc_128(Key, WhatToSign),
  hex(Signed).
  
verify_signature(Data, Key, Signature, Date) ->
  ExpectedSig = create_signature(Data, Key, Date),
  case Signature =:= ExpectedSig of
    true ->
      {valid};
    false ->
      {invalid, {expected, ExpectedSig}, {received, Signature}}
  end.

validate_input(client, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(client_string, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(system, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(sub_system, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;


validate_input(auth, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\,\\.\\:\\-\\+\\|]{55,200})$") of
    {match,_Match} ->
     valid;
    _ELSE -> invalid
  end;

validate_input(auth_delimiter, "|") ->
  valid;
validate_input(auth_delimiter, ",") ->
  valid;
validate_input(auth_delimiter, "||") ->
  valid;
validate_input(auth_delimiter, _Input) ->
  invalid;

validate_input(tags, Input) ->
  ParsedTags = string:tokens(Input, ", "),
  ValidationList = validate_tags(ParsedTags),
  case proplists:is_defined(invalid, ValidationList) of
    true ->
     invalid;
    false -> 
     valid
  end;

validate_input(url, Input) when length(Input) < 201 ->
  case re:run(Input, "^((https?)://)[A-Za-z0-9-]+(\.[A-Za-z0-9-]+)+([/?].*)?$") of
    {match,_Match} ->
     valid;
    _ELSE -> invalid
  end;

validate_input(url, Input) when length(Input) > 200 ->
  invalid;

validate_input(normal, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\:\\.\\+\\-]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end;

validate_input(content_type, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\.\\+\\-\\/\\;\\= ]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end;

validate_input(realm, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\.\\*]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end.

validate_tags(Tags) ->
  validate_tags(Tags, []).

validate_tags([], TagList) ->
  TagList;
validate_tags([Tag|T], TagList) ->
  case re:run(Tag, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     [{valid,Tag}|validate_tags(T, TagList)];
    _ELSE -> 
     [{invalid,Tag}|validate_tags(T, TagList)]
  end.

validate_all_inputs([]) ->
  valid;  

validate_all_inputs([{Input, Description, Type}|TheRest]) ->
  case validate_input(Type, Input) of
    valid ->
      validate_all_inputs(TheRest);
    invalid ->
      {invalid, Description}
  end.
  


