package pl.mzlnk.emergencyspot.service.network.requests;

import com.google.gson.reflect.TypeToken;

import org.json.JSONObject;

import java.util.List;


public interface HttpRequestParams<T> {

    int getRequestMethod();

    String getUrl();

    JSONObject getRequestBody();

    boolean isAuthorized();

    Class<T> receivedDataClass();

    TypeToken<List<T>> receivedDataTypeToken();

}
