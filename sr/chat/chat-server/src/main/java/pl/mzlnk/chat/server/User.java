package pl.mzlnk.chat.server;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.net.InetAddress;
import java.net.Socket;

@Data
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class User {

    private final Socket socket;

    @EqualsAndHashCode.Include
    private final InetAddress host;
    @EqualsAndHashCode.Include
    private final int port;

    private String name;

    public User(Socket socket) {
        this.socket = socket;
        this.host = socket.getInetAddress();
        this.port = socket.getPort();
    }

    public String getName() {
        return this.name != null ? (this.name) : "";
    }

    @Override
    public String toString() {
        return String.format("%s[%s:%d]", this.getName(), this.socket.getInetAddress(), this.socket.getPort());
    }

}
