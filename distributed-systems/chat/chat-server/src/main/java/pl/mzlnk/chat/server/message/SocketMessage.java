package pl.mzlnk.chat.server.message;

import lombok.Data;

@Data
public class SocketMessage {

    private SocketMessageType type;
    private String data;

}
