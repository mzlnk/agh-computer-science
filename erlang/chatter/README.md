# agh-erl-chatter

Local chat application built with Java + jInterface (client) and Erlang (server). This is the part of the project for 
*Programming in Erlang and Elixir* course which took place on 2nd year of Computer Science at the faculty of Computer Science,
Electronics and Telecomunications at AGH UST.

Project author:
- Marcin Zielonka

## Build and run

### Client

Client is written in Java 14 using JavaFX and built with Maven. To build the application you have to run following command 
in client's project root:

```text
mvn clean package assembly:single
```

The `jar` file with application will be generated in `/target` directory.

To run the application you have to run following command in client's project root:

```text
java -jar --enable-preview target/ErlChatter-1.0.jar
```

### Server

Server is written in Erlang and built with Rebar. To build and run server application you have to run following command 
in server's project root:

```text
rebar3 shell --sname chatServer --setcookie erlangcookie
```
