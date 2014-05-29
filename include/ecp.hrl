-record(ecp_principal, {id, friendly_name, enforced_tag_ids=[], is_require_message_type_with_new_subs=false, date_created, date_deactivated, secret, realm="*", delivery_url_mask="*"}).

-record(ecp_sub, {id, principal_id, callback_url, wsdl_uri, queue_name, tag_ids=[], date_created, date_cancelled, duplication_key}).

-record(ecp_tag, {id, type, value}). %% Note:  Changing fields here means you have to also update the object queries in pe_tag_store.erl.

-record(ecp_kvpair, {key, value}).

-record(message_key, {client, clientString, messageType, system, subSystem}).

-record(status, {node, version, num_messages_posted, num_messages_delivered, num_failed_deliveries, time_started, uptime, num_watched_subscriptions, subscription_pids, pids_awaiting_broker_reconnection=[], rest_status}).

-define(CLIENT_TAG_TYPE, "Client").
-define(CLIENT_STRING_TAG_TYPE, "ClientString").
-define(MESSAGE_TYPE_TAG_TYPE, "MessageType").
-define(SYSTEM_TAG_TYPE, "System").
-define(SUB_SYSTEM_TAG_TYPE, "SubSystem").
