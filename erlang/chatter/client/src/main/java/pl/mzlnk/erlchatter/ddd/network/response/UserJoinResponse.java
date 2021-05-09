package pl.mzlnk.erlchatter.ddd.network.response;

import com.ericsson.otp.erlang.OtpErlangTuple;
import lombok.Getter;
import pl.mzlnk.erlchatter.ddd.utils.OtpErlangObjectDto;

import java.util.Calendar;

@Getter
public class UserJoinResponse extends BaseNetworkResponse {

    public static UserJoinResponse fromTuple(OtpErlangTuple tuple) {
        String who = OtpErlangObjectDto.fromObject(tuple.elementAt(1)).getStringValue();

        Calendar date = Calendar.getInstance();

        OtpErlangTuple dateTuple = (OtpErlangTuple) ((OtpErlangTuple) tuple.elementAt(2)).elementAt(0);
        OtpErlangTuple timeTuple = (OtpErlangTuple) ((OtpErlangTuple) tuple.elementAt(2)).elementAt(1);

        date.set(Calendar.YEAR, (int) OtpErlangObjectDto.fromObject(dateTuple.elementAt(0)).getLongValue());
        date.set(Calendar.MONTH, (int) OtpErlangObjectDto.fromObject(dateTuple.elementAt(1)).getLongValue() - 1);
        date.set(Calendar.DAY_OF_MONTH, (int) OtpErlangObjectDto.fromObject(dateTuple.elementAt(2)).getLongValue());

        date.set(Calendar.HOUR_OF_DAY, (int) OtpErlangObjectDto.fromObject(timeTuple.elementAt(0)).getLongValue());
        date.set(Calendar.MINUTE, (int) OtpErlangObjectDto.fromObject(timeTuple.elementAt(1)).getLongValue());
        date.set(Calendar.SECOND, (int) OtpErlangObjectDto.fromObject(timeTuple.elementAt(2)).getLongValue());

        return new UserJoinResponse(who, date);
    }

    private String who;
    private Calendar date;

    public UserJoinResponse(String who, Calendar date) {
        super(ResponseTypeEnum.USER_JOIN);
        this.who = who;
        this.date = date;
    }

}
