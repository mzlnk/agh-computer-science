package pl.mzlnk.erlchatter.ddd.network.request;

import lombok.ToString;
import pl.mzlnk.erlchatter.ddd.user.User;

@ToString(callSuper = true)
public class SignOutRequest extends BaseAuthorizedNetworkRequest {

    public SignOutRequest(User user) {
        super(user, RequestTypeEnum.USER_SIGN_OUT);
    }

    @Override
    protected Object[] args() {
        return new Object[]{super.user.getToken()};
    }

}
