package pl.mzlnk.emergencyspot.service.user;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspot.model.user.AuthUserDto;
import pl.mzlnk.emergencyspot.service.network.requests.UserSignInHttpRequestParams;

import static pl.mzlnk.emergencyspot.EmergencySpotApplication.app;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UserService {

    private AuthUserDto authUser;

    public static UserService init() {
        return new UserService();
    }

    public AuthUserDto getAuthUser() {
        return authUser;
    }

    public boolean isUserSignedIn() {
        return authUser != null;
    }

    public void signIn(String username, String password) {
        app.networkService.makeRequestForObject(
                new UserSignInHttpRequestParams(),
                authUser -> {
                    this.authUser = authUser;
                },
                error -> {
                    throw new RuntimeException("Could not sign in. Error: " + error);
                }
        );
    }

    public void signOut() {
        this.authUser = null;
    }

}
