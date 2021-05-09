package pl.mzlnk.emergencyspot.ui.fragment;

import android.os.Bundle;
import android.view.View;
import android.widget.LinearLayout;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;
import pl.mzlnk.emergencyspot.model.hospital.HospitalParamsDto;
import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspot.service.network.requests.HttpRequestParams;
import pl.mzlnk.emergencyspot.service.network.requests.RetrieveHospitalWardsHttpRequestParams;
import pl.mzlnk.emergencyspot.service.network.requests.RetrieveHospitalsHttpRequestParams;
import pl.mzlnk.emergencyspot.ui.asynctask.HospitalWardsOverviewLoadViewsTask;
import pl.mzlnk.emergencyspot.ui.asynctask.HospitalsOverviewLoadViewsTask;
import pl.mzlnk.emergencyspot.ui.asynctask.LoadViewsTask;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalWardListItemView;

public class HospitalWardsOverviewFragment extends BaseNetworkDataLoadViewsReceiverFragment<HospitalWardDto, Long, HospitalWardListItemView> {

    private static final String HOSPITAL_ID_PARAM = "f.hospital.ward.overview.hospital.id";

    private LinearLayout hospitalsContainer;

    private Long hospitalId;

    public HospitalWardsOverviewFragment() {
        super();
    }

    public static HospitalWardsOverviewFragment newInstance(Long hospitalId) {
        HospitalWardsOverviewFragment fragment = new HospitalWardsOverviewFragment();

        Bundle args = new Bundle();
        args.putLong(HOSPITAL_ID_PARAM, hospitalId);
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    protected void readArguments(Bundle arguments) {
        this.hospitalId = arguments.getLong(HOSPITAL_ID_PARAM);
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_hospital_wards_overview;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        this.hospitalsContainer = rootView.findViewById(R.id.f_hospital_wards_overview_layout_hospital_wards_container);
    }

    @Override
    public LoadViewsTask<HospitalWardDto> loadViewsTask() {
        return new HospitalWardsOverviewLoadViewsTask(this);
    }

    @Override
    public LinearLayout getContainer() {
        return hospitalsContainer;
    }

    @Override
    public HttpRequestParams<HospitalWardDto> httpRequestParams() {
        return new RetrieveHospitalWardsHttpRequestParams(this.hospitalId);
    }

}
