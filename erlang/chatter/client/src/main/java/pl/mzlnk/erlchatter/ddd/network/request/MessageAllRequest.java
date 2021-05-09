package pl.mzlnk.erlchatter.ddd.network.request;

import pl.mzlnk.erlchatter.ddd.user.User;

public class MessageAllRequest extends BaseAuthorizedNetworkRequest {

    private String message;

    public MessageAllRequest(String message, User user) {
        super(user, RequestTypeEnum.MESSAGE_ALL);
        this.message = message;
    }

    @Override
    protected Object[] args() {
        return new Object[]{super.user.getToken(), this.message};
    }

}
