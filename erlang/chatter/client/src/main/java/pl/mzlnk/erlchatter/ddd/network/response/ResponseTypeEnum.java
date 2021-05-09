package pl.mzlnk.erlchatter.ddd.network.response;

import java.util.stream.Stream;

public enum ResponseTypeEnum {

    ERROR,
    MESSAGE_ALL,
    MESSAGE_TO,
    USER_SIGN_IN,
    USER_SIGN_UP,
    USER_SIGN_OUT,
    USER_JOIN,
    USER_LEAVE;

    public static boolean contains(String name) {
        return Stream.of(values())
                .anyMatch(e -> e.name().toLowerCase().equals(name));
    }

}
