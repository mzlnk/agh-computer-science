package pl.mzlnk.erlchatter.ddd.user;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;

@Getter
@AllArgsConstructor
@ToString
public class User {

    private String login;
    private String token;

}
