package pl.mzlnk.chat.server.tcp;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import pl.mzlnk.chat.server.User;
import pl.mzlnk.chat.server.message.Message;

import java.io.PrintWriter;
import java.util.Queue;
import java.util.Set;

@Slf4j
@RequiredArgsConstructor
public class TcpTransmitter implements Runnable {

    private final Queue<Message> queue;
    private final Set<User> users;

    @Override
    public void run() {
        log.info("Started TCP message sender service");
        while (true) {
            if (!queue.isEmpty()) {
                Message message = queue.poll();
                log.info("Received message: {}", message.getMessage());

                users.stream()
                        .filter(user -> !user.equals(message.getUser()))
                        .forEach(user -> sendMessage(user, message));
            }
        }
    }

    @SneakyThrows
    private void sendMessage(User user, Message message) {
        log.info("Sending message to: {}", user);
        PrintWriter out = new PrintWriter(user.getSocket().getOutputStream(), true);
        out.println(message.getMessage());
    }

}
