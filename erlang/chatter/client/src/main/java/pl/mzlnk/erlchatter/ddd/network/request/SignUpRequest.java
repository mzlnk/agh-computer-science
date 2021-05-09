package pl.mzlnk.erlchatter.ddd.network.request;

public class SignUpRequest extends BaseNetworkRequest {

    private final String login;
    private final String password;

    public SignUpRequest(String login, String password) {
        super(RequestTypeEnum.USER_SIGN_UP);
        this.login = login;
        this.password = password;
    }

    @Override
    protected Object[] args() {
        return new Object[]{this.login, this.password};
    }

}
