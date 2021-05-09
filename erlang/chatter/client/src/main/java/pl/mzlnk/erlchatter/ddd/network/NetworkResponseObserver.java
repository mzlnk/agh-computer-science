package pl.mzlnk.erlchatter.ddd.network;

import pl.mzlnk.erlchatter.ddd.network.response.*;

public interface NetworkResponseObserver {

    default void onResponse(ErrorResponse response) {}
    default void onResponse(MessageAllResponse response) {}
    default void onResponse(SignInResponse response) {}
    default void onResponse(SignUpResponse response) {}
    default void onResponse(SignOutResponse response) {}
    default void onResponse(UserLeaveResponse response) {}
    default void onResponse(UserJoinResponse response) {}

}
