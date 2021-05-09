package pl.mzlnk.erlchatter.ddd.network.response;

import com.ericsson.otp.erlang.OtpErlangTuple;
import lombok.Getter;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangObjectDto;

@Getter
public class SignUpResponse extends BaseNetworkResponse {

    public static SignUpResponse fromTuple(OtpErlangTuple tuple) {
        String login = OtpErlangObjectDto.fromObject(tuple.elementAt(1)).getStringValue();

        return new SignUpResponse(login);
    }

    private String login;

    public SignUpResponse(String login) {
        super(ResponseTypeEnum.USER_SIGN_UP);
        this.login = login;
    }

}
