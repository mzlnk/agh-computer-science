package pl.mzlnk.emergencyspot.service.network;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.Volley;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspot.model.user.AuthUserDto;
import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class NetworkService {

    private RequestQueue requestQueue;

    public static NetworkService init(Context context) {
        NetworkService networkService = new NetworkService();

        networkService.requestQueue = Volley.newRequestQueue(context);

        return networkService;
    }

    public <T> void makeRequestForObject(HttpRequestParams<T> requestParams,
                                         Response.Listener<T> onSuccessListener,
                                         Response.ErrorListener errorListener) {

        requestQueue.add(new HttpObjectRequest<>(requestParams, onSuccessListener, errorListener));
    }

    public <T> void makeAuthorizedRequestForObject(HttpRequestParams<T> requestParams,
                                                   Response.Listener<T> onSuccessListener,
                                                   Response.ErrorListener errorListener,
                                                   AuthUserDto authUser) {

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer: " + authUser.getToken());

        requestQueue.add(new HttpObjectRequest<>(requestParams, onSuccessListener, errorListener, headers));
    }

    public <T> void makeRequestForList(HttpRequestParams<T> requestParams,
                                       Response.Listener<List<T>> onSuccessListener,
                                       Response.ErrorListener errorListener) {

        requestQueue.add(new HttpListRequest<>(requestParams, onSuccessListener, errorListener));
    }

    public <T> void makeAuthorizedRequestForList(HttpRequestParams<T> requestParams,
                                                 Response.Listener<List<T>> onSuccessListener,
                                                 Response.ErrorListener errorListener,
                                                 AuthUserDto authUser) {

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer: " + authUser.getToken());

        requestQueue.add(new HttpListRequest<>(requestParams, onSuccessListener, errorListener, headers));
    }

}
