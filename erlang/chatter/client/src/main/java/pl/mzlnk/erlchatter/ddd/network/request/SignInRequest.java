package pl.mzlnk.erlchatter.ddd.network.request;

import com.ericsson.otp.erlang.OtpErlangPid;
import lombok.ToString;

@ToString
public class SignInRequest extends BaseNetworkRequest {

    private final String login;
    private final String password;
    private final OtpErlangPid pid;

    public SignInRequest(String login, String password, OtpErlangPid pid) {
        super(RequestTypeEnum.USER_SIGN_IN);
        this.login = login;
        this.password = password;
        this.pid = pid;
    }

    @Override
    protected Object[] args() {
        return new Object[]{this.login, this.password, this.pid};
    }

}
