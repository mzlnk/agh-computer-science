package pl.mzlnk.emergencyspot.ui.asynctask;

import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;

public interface NetworkDataReceiver<T> {

    HttpRequestParams<T> httpRequestParams();

    void onDataReceivedFromNetwork(T data);

}
