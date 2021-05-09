package pl.mzlnk.erlchatter.ddd.network;

import com.ericsson.otp.erlang.*;
import pl.mzlnk.erlchatter.ddd.network.request.NetworkRequest;
import pl.mzlnk.erlchatter.ddd.network.response.*;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangObjectDto;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public class NetworkService {

    private static final String COOKIE = "erlangcookie";

    private final UUID nodeUuid;

    private OtpSelf client;
    private OtpPeer server;
    private OtpConnection connection;

    private List<NetworkResponseObserver> responseObservers = new ArrayList<>();
    private volatile boolean running;

    public NetworkService() {
        this.nodeUuid = UUID.randomUUID();
    }

    public void start() throws IOException, OtpAuthException {
        client = new OtpSelf(nodeUuid.toString().replaceAll("-", ""), COOKIE);
        server = new OtpPeer("chatServer");
        connection = client.connect(server);

        this.running = true;
        new Thread(new ResponseReceiveTask()).start();

        System.out.println("User pid: " + Optional.ofNullable(this.client.pid()).map(OtpErlangPid::toString).orElse("null!"));
    }

    public void stop() {
        this.running = false;
    }

    public void addResponseObserver(NetworkResponseObserver observer) {
        this.responseObservers.add(observer);
    }

    public void removeResponseObserver(NetworkResponseObserver observer) {
        this.responseObservers.remove(observer);
    }

    public void sendRequest(NetworkRequest request) {
        try {
            connection.sendRPC("erlchatter_gen_server", request.getRequestType().name().toLowerCase(), request.getArgs());
        } catch (IOException | NullPointerException e) {
            System.out.println("Could not send request");
            e.printStackTrace();
        }
    }

    public OtpErlangPid getPid() {
        return client.pid();
    }

    private class ResponseReceiveTask implements Runnable {

        @Override
        public void run() {
            while(true) {
                if(!NetworkService.this.running) {
                    connection.close();

                    return;
                }

                try {
                    OtpErlangObject response = ((OtpErlangTuple) connection.receiveMsg().getMsg()).elementAt(1);

                    if(response instanceof OtpErlangTuple tuple) {
                        ResponseTypeEnum responseType = OtpErlangObjectDto.fromObject(tuple.elementAt(0)).getResponseTypeEnumValue();

                        switch (responseType) {
                            case ERROR -> responseObservers.forEach(o -> o.onResponse(ErrorResponse.fromTuple(tuple)));
                            case USER_SIGN_IN -> responseObservers.forEach(o -> o.onResponse(SignInResponse.fromTuple(tuple)));
                            case USER_SIGN_UP -> responseObservers.forEach(o -> o.onResponse(SignUpResponse.fromTuple(tuple)));
                            case USER_SIGN_OUT -> responseObservers.forEach(o -> o.onResponse(new SignOutResponse()));
                            case MESSAGE_ALL -> responseObservers.forEach(o -> o.onResponse(MessageAllResponse.fromTuple(tuple)));
                            case USER_JOIN -> responseObservers.forEach(o -> o.onResponse(UserJoinResponse.fromTuple(tuple)));
                            case USER_LEAVE -> responseObservers.forEach(o -> o.onResponse(UserLeaveResponse.fromTuple(tuple)));
                        }
                    }
                } catch (OtpErlangExit | IOException | OtpAuthException | OtpErlangDecodeException e) {
                    e.printStackTrace();
                }
            }
        }

    }

}
