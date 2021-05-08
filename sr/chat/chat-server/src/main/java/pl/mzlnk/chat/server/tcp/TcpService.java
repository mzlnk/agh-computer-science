package pl.mzlnk.chat.server.tcp;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import pl.mzlnk.chat.server.User;
import pl.mzlnk.chat.server.message.Message;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.HashSet;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;

import static pl.mzlnk.chat.server.Application.submitTask;

@Slf4j
public class TcpService implements Runnable {

    private static final int MAX_MESSAGE_QUEUE_CAPACITY = 100;

    private final int port;

    private final Queue<Message> queue = new ArrayBlockingQueue<>(MAX_MESSAGE_QUEUE_CAPACITY);
    private final Set<User> users = Collections.synchronizedSet(new HashSet<>());

    public TcpService(int port) {
        this.port = port;
    }

    @Override
    public void run() {
        try (ServerSocket serverSocket = new ServerSocket(this.port)) {
            log.info("Started TCP service");
            TcpTransmitter sender = new TcpTransmitter(queue, users);
            submitTask(sender);

            listenForNewClients(serverSocket);
        } catch (IOException e) {
            log.error("TCP server socket closed unexpectedly");
        }

    }

    @SneakyThrows
    private void listenForNewClients(ServerSocket serverSocket) {
        log.info("Started listening for new users");
        while (true) {
            log.info("Waiting for new user...");
            Socket client = serverSocket.accept();

            User user = new User(client);
            log.info("New user: {}", user);

            this.users.add(user);

            TcpListener receiver = new TcpListener(user, queue, () -> this.users.remove(user));
            submitTask(receiver);
        }
    }

}
