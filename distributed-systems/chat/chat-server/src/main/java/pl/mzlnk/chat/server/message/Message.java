package pl.mzlnk.chat.server.message;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import pl.mzlnk.chat.server.User;

@Data
@RequiredArgsConstructor
public class Message {

    private final User user;
    private final String message;

}
