package pl.mzlnk.emergencyspot.service.network.requests;

import com.google.gson.reflect.TypeToken;

import org.json.JSONObject;

import java.util.List;

public abstract class BaseHttpRequestParams<T> implements HttpRequestParams<T> {

    @Override
    public JSONObject getRequestBody() {
        return null;
    }

    @Override
    public boolean isAuthorized() {
        return false;
    }

    @Override
    public Class<T> receivedDataClass() {
        return null;
    }

    @Override
    public TypeToken<List<T>> receivedDataTypeToken() {
        return null;
    }

}
