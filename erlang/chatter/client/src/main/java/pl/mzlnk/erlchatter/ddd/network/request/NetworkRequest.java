package pl.mzlnk.erlchatter.ddd.network.request;

import com.ericsson.otp.erlang.OtpErlangList;

public interface NetworkRequest {

    RequestTypeEnum getRequestType();

    OtpErlangList getArgs();

}
