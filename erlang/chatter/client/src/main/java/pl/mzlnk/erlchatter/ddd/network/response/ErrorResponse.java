package pl.mzlnk.erlchatter.ddd.network.response;

import com.ericsson.otp.erlang.OtpErlangTuple;
import lombok.Getter;
import pl.mzlnk.erlchatter.ddd.network.request.RequestTypeEnum;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangObjectDto;

@Getter
public class ErrorResponse extends BaseNetworkResponse {

    public static ErrorResponse fromTuple(OtpErlangTuple tuple) {
        RequestTypeEnum from = OtpErlangObjectDto.fromObject(tuple.elementAt(1)).getRequestTypeEnumValue();
        String message = OtpErlangObjectDto.fromObject(tuple.elementAt(2)).getStringValue();

        return new ErrorResponse(from, message);
    }

    private RequestTypeEnum from;
    private String message;

    public ErrorResponse(RequestTypeEnum from, String message) {
        super(ResponseTypeEnum.ERROR);
        this.from = from;
        this.message = message;
    }

}
