package pl.mzlnk.emergencyspot.ui.fragment;

import android.view.View;
import android.widget.LinearLayout;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;
import pl.mzlnk.emergencyspot.model.hospital.HospitalParamsDto;
import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;
import pl.mzlnk.emergencyspot.service.network.requests.RetrieveHospitalsHttpRequestParams;
import pl.mzlnk.emergencyspot.ui.asynctask.HospitalsOverviewLoadViewsTask;
import pl.mzlnk.emergencyspot.ui.asynctask.LoadViewsTask;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalListItemView;


public class HospitalsOverviewFragment extends BaseNetworkDataLoadViewsReceiverFragment<HospitalDto, Long, HospitalListItemView> {

    private LinearLayout hospitalsContainer;

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_hospitals_overview;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        this.hospitalsContainer = rootView.findViewById(R.id.f_hospitals_overview_layout_hospitals_container);
    }

    @Override
    public LoadViewsTask<HospitalDto> loadViewsTask() {
        return new HospitalsOverviewLoadViewsTask(this);
    }

    @Override
    public LinearLayout getContainer() {
        return hospitalsContainer;
    }

    @Override
    public HttpRequestParams<HospitalDto> httpRequestParams() {
        return new RetrieveHospitalsHttpRequestParams(new HospitalParamsDto());
    }

}
