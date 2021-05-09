package pl.mzlnk.erlchatter.ddd.user;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class UserService {

    private User user;

    public void signIn(String login, String token) {
        this.user = new User(login, token);
    }

    public void signOut() {
        this.user = null;
    }

}
