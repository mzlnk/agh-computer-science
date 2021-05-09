package pl.mzlnk.erlchatter.ddd.utils;

import com.ericsson.otp.erlang.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.SneakyThrows;
import pl.mzlnk.erlchatter.ddd.network.request.RequestTypeEnum;
import pl.mzlnk.erlchatter.ddd.network.response.ResponseTypeEnum;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class OtpErlangObjectDto {

    private String stringValue;
    private long longValue;
    private int intValue;
    private RequestTypeEnum requestTypeEnumValue;
    private ResponseTypeEnum responseTypeEnumValue;

    @SneakyThrows
    public static OtpErlangObjectDto fromObject(OtpErlangObject obj) {
        OtpErlangObjectDto dto = new OtpErlangObjectDto();

        if (obj instanceof OtpErlangString sObj) {
            dto.stringValue = sObj.stringValue();
        }

        if (obj instanceof OtpErlangLong lObj) {
            dto.longValue = lObj.longValue();
        }

        if (obj instanceof OtpErlangInt iObj) {
            dto.intValue = iObj.intValue();
        }

        if (obj instanceof OtpErlangAtom aObj) {
            if (ResponseTypeEnum.contains(aObj.atomValue())) {
                dto.responseTypeEnumValue = ResponseTypeEnum.valueOf(aObj.atomValue().toUpperCase());
            }
            if (RequestTypeEnum.contains(aObj.atomValue())) {
                dto.requestTypeEnumValue = RequestTypeEnum.valueOf(aObj.atomValue().toUpperCase());
            }
        }

        return dto;
    }

}
