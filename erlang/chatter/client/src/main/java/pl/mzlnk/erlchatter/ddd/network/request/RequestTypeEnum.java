package pl.mzlnk.erlchatter.ddd.network.request;

import java.util.stream.Stream;

public enum RequestTypeEnum {

    USER_SIGN_IN,
    USER_SIGN_UP,
    USER_SIGN_OUT,
    MESSAGE_ALL;

    public static boolean contains(String name) {
        return Stream.of(values())
                .anyMatch(e -> e.name().toLowerCase().equals(name));
    }

}
