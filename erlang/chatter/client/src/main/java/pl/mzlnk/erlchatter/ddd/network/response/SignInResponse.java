package pl.mzlnk.erlchatter.ddd.network.response;

import com.ericsson.otp.erlang.OtpErlangTuple;
import lombok.Getter;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangObjectDto;

@Getter
public class SignInResponse extends BaseNetworkResponse {

    public static SignInResponse fromTuple(OtpErlangTuple tuple) {
        String login = OtpErlangObjectDto.fromObject(tuple.elementAt(1)).getStringValue();
        String token = OtpErlangObjectDto.fromObject(tuple.elementAt(2)).getStringValue();

        return new SignInResponse(login, token);
    }

    private String login;
    private String token;

    public SignInResponse(String login, String token) {
        super(ResponseTypeEnum.USER_SIGN_IN);
        this.login = login;
        this.token = token;
    }

}
