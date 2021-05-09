package pl.mzlnk.emergencyspot.ui.fragment;

import android.view.View;
import android.widget.Toast;

import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;
import pl.mzlnk.emergencyspot.ui.asynctask.NetworkDataReceiver;

import static pl.mzlnk.emergencyspot.EmergencySpotApplication.app;

public abstract class BaseNetworkDataReceiverFragment<T> extends BaseFragment implements NetworkDataReceiver<T> {

    @Override
    protected void prepareLayout(View rootView) {
        super.prepareLayout(rootView);

        retrieveNetworkData();
    }

    private void retrieveNetworkData() {
        HttpRequestParams<T> params = httpRequestParams();
        if (params.isAuthorized()) {
            app.networkService.makeAuthorizedRequestForObject(
                    params,
                    this::onDataReceivedFromNetwork,
                    error -> Toast.makeText(getContext(), "Could not retrieve data from server", Toast.LENGTH_SHORT).show(),
                    app.userService.getAuthUser()
            );
        } else {
            app.networkService.makeRequestForObject(
                    params,
                    this::onDataReceivedFromNetwork,
                    error -> Toast.makeText(getContext(), "Could not retrieve data from server,", Toast.LENGTH_SHORT).show()
            );
        }
    }

}
