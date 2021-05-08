package pl.mzlnk.chat.server.tcp;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import pl.mzlnk.chat.server.User;
import pl.mzlnk.chat.server.message.Message;
import pl.mzlnk.chat.server.message.SocketMessage;
import pl.mzlnk.chat.server.message.SocketMessageType;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Queue;

@Slf4j
public class TcpListener implements Runnable {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private final User user;
    private final Queue<Message> queue;

    private final Runnable onInterruptCallback;


    public TcpListener(User user, Queue<Message> queue, Runnable onInterruptCallback) {
        this.user = user;
        this.queue = queue;
        this.onInterruptCallback = onInterruptCallback;
    }

    @Override
    public void run() {
        while (true) {
            try {
                BufferedReader in = new BufferedReader(new InputStreamReader(user.getSocket().getInputStream()));
                String raw = in.readLine();

                if (raw == null) {
                    throw new InterruptedException("Connection closed");
                }

                SocketMessage socketMessage = objectMapper.readValue(raw, SocketMessage.class);
                if(socketMessage.getType() == SocketMessageType.ATTACH_NAME) {
                    this.user.setName(socketMessage.getData());
                    sendJoinMessage();

                    continue;
                }

                Message message = new Message(this.user, socketMessage.getData());
                queue.add(message);

            } catch (JsonProcessingException e) {
                log.warn("Could not parse incoming message");
            } catch (InterruptedException | IOException e) {
                log.info("Connection with client {} closed", this.user);
                sendQuitMessage();

                onInterruptCallback.run();
                return;
            }
        }
    }

    private void sendQuitMessage() {
        Message message = new Message(this.user, String.format("User %s left", this.user.getName()));
        queue.add(message);
    }

    private void sendJoinMessage() {
        Message message = new Message(this.user, String.format("User %s joined", this.user.getName()));
        queue.add(message);
    }


}
