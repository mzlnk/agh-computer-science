package pl.mzlnk.erlchatter.ddd.network.request;

import com.ericsson.otp.erlang.OtpErlangList;
import lombok.AllArgsConstructor;
import lombok.Getter;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangListCreator;

@Getter
@AllArgsConstructor
public abstract class BaseNetworkRequest implements NetworkRequest {

    private RequestTypeEnum requestType;

    protected abstract Object[] args();

    @Override
    public OtpErlangList getArgs() {
        return OtpErlangListCreator.toList(args());
    }

}
