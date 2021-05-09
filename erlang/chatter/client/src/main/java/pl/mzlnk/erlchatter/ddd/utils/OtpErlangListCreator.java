package pl.mzlnk.erlchatter.ddd.utils;

import com.ericsson.otp.erlang.*;

import java.util.stream.Stream;

public class OtpErlangListCreator {

    public static OtpErlangList toList(Object[] objects) {
        return new OtpErlangList(
                Stream.of(objects)
                        .map(obj -> {
                                    OtpErlangObject otpObject = null;

                                    if (obj instanceof String s) {
                                        otpObject = new OtpErlangString(s);
                                    }
                                    if (obj instanceof Long l) {
                                        otpObject = new OtpErlangLong(l);
                                    }
                                    if (obj instanceof Enum<?> e) {
                                        otpObject = new OtpErlangAtom(e.name().toLowerCase());
                                    }
                                    if (obj instanceof Double d) {
                                        otpObject = new OtpErlangDouble(d);
                                    }
                                    if(obj instanceof OtpErlangPid pid) {
                                        otpObject = pid;
                                    }

                                    return otpObject;
                                }
                        )
                        .toArray(OtpErlangObject[]::new)
        );
    }

}
