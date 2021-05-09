package pl.mzlnk.erlchatter.ddd.network.response;

import com.ericsson.otp.erlang.OtpErlangTuple;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public abstract class BaseNetworkResponse {

    private final ResponseTypeEnum responseType;

}
