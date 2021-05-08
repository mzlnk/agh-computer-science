package pl.mzlnk.chat.server;

import lombok.extern.slf4j.Slf4j;
import pl.mzlnk.chat.server.tcp.TcpService;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
public class Application {

    private static final ExecutorService poolService = Executors.newCachedThreadPool();

    public static void main(String[] args) throws Exception {
        final int port = 12345;

        TcpService tcpService = new TcpService(port);
        submitTask(tcpService);
    }

    public static void submitTask(Runnable task) {
        poolService.submit(task);
    }

}
