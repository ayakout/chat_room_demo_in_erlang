{application, chat_room,
[{vsn, "1.0.0"},
{modules, [chat_client, chat_client_sup, chat_room, chat_room_sup, chat_server, message_bus, message_history]},
{registered, [chat_room]},
{mod, {chat_room, []}}
]}.
