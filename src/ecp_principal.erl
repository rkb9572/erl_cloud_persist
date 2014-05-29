-module(ecp_principal).

-export([
	get/2, 
	instance/8, 
	instance/9, 
	is_url_within_mask/2,
	to_string/1
]).

-include("include/ecp.hrl").

get(id, Principal) ->
  Principal#ecp_principal.id;
  
get(friendly_name, Principal) ->
  Principal#ecp_principal.friendly_name;

get(enforced_tag_ids, Principal) ->
  Principal#ecp_principal.enforced_tag_ids;

get(is_require_message_type_with_new_subs, Principal) ->
  Principal#ecp_principal.is_require_message_type_with_new_subs;

get(date_created, Principal) ->
  Principal#ecp_principal.date_created;

get(date_deactivated, Principal) ->
  Principal#ecp_principal.date_deactivated;

get(secret, Principal) ->
  Principal#ecp_principal.secret;

get(realm, Principal) ->
  Principal#ecp_principal.realm;
  
get(delivery_url_mask, Principal) ->
  Principal#ecp_principal.delivery_url_mask.

instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, Realm, DeliveryUrlMask) ->
  instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, undefined, Realm, DeliveryUrlMask).

instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, DateDeactivated, Realm, DeliveryUrlMask) ->
  #ecp_principal.
    id=PrincipalId, 
    friendly_name=FriendlyName, 
    enforced_tag_ids=TagRecords, 
    is_require_message_type_with_new_subs=RequireMessageTypeWithSubscription,
    date_created=DateCreated,
    date_deactivated=DateDeactivated,
    secret=Secret,
    realm=Realm,
    delivery_url_mask=DeliveryUrlMask
  }.

%
% convert a ecp_principal.record/tuple to a string
%
to_string (Principal) ->
	io_lib:format(
		"{ ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p }", 
		[ 
			Principal#ecp_principal.id,
			Principal#ecp_principal.friendly_name,
			Principal#ecp_principal.enforced_tag_ids,
			Principal#ecp_principal.is_require_message_type_with_new_subs,
			Principal#ecp_principal.date_created,
			Principal#ecp_principal.date_deactivated,
			Principal#ecp_principal.secret,
			Principal#ecp_principal.realm,
			Principal#ecp_principal.delivery_url_mask
		]
	).

