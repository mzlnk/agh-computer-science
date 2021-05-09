package pl.mzlnk.emergencyspot.service.network;

import android.util.Log;

import com.android.volley.AuthFailureError;
import com.android.volley.NetworkResponse;
import com.android.volley.ParseError;
import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.HttpHeaderParser;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import java.io.UnsupportedEncodingException;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;

public class HttpListRequest<T> extends Request<List<T>> {

    private final Type listType;
    private final Response.Listener<List<T>> onSuccessListener;

    private final Map<String, String> headers;


    public HttpListRequest(HttpRequestParams<T> requestParams,
                           Response.Listener<List<T>> onSuccessListener,
                           Response.ErrorListener onErrorListener) {
        this(requestParams, onSuccessListener, onErrorListener, null);
    }

    public HttpListRequest(HttpRequestParams<T> requestParams,
                           Response.Listener<List<T>> onSuccessListener,
                           Response.ErrorListener onErrorListener,
                           Map<String, String> headers) {

        super(requestParams.getRequestMethod(), requestParams.getUrl(), onErrorListener);

        this.listType = requestParams.receivedDataTypeToken().getType();
        this.onSuccessListener = onSuccessListener;
        this.headers = headers;
    }

    @Override
    public Map<String, String> getHeaders() throws AuthFailureError {
        return headers != null ? headers : super.getHeaders();
    }

    @Override
    protected void deliverResponse(List<T> response) {
        this.onSuccessListener.onResponse(response);
    }

    @Override
    protected Response<List<T>> parseNetworkResponse(NetworkResponse response) {
        Gson gson = new Gson();

        try {
            String json = new String(
                    response.data,
                    HttpHeaderParser.parseCharset(response.headers));

            Log.d("network", "json response:\n" + json);

            return Response.success(
                    gson.fromJson(json, this.listType),
                    HttpHeaderParser.parseCacheHeaders(response));
        } catch (UnsupportedEncodingException | JsonSyntaxException e) {
            return Response.error(new ParseError(e));
        }
    }


}
