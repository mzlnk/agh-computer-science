package pl.mzlnk.emergencyspot.service.network.requests;

import pl.mzlnk.emergencyspot.model.user.AuthUserDto;

public class UserSignInHttpRequestParams extends BaseHttpRequestParams<AuthUserDto> {

    @Override
    public int getRequestMethod() {
        return 0;
    }

    @Override
    public String getUrl() {
        return null;
    }
}
