Chat room demo in Erlang
=============
Demo files from blog post by Ali Yakout
http://ayakout.blogspot.com/2015/07/chat-room-demo-in-erlang.html

Running
-----------
### Server
```
erlc -o ebin src/*.erl
erl -s chat_room -pa ebin
```

### Client
```
./src/chat_room.es
```
or simply `
```
telnet 127.0.0.1 6667
```
@Copyright (C) 2015, Ali Yakout
