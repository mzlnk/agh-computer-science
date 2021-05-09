package pl.mzlnk.emergencyspot.ui.asynctask;

import java.util.List;

import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalListItemView;
import pl.mzlnk.emergencyspot.ui.viewprovider.HospitalsOverviewViewProvider;
import pl.mzlnk.emergencyspot.ui.viewprovider.ViewProvider;

public class HospitalsOverviewLoadViewsTask extends BaseLoadViewsTask<HospitalDto, Long, HospitalListItemView> {

    public HospitalsOverviewLoadViewsTask(LoadViewsTaskReceiver<HospitalDto, Long, HospitalListItemView> receiver) {
        super(receiver);
    }

    @Override
    protected ViewProvider<HospitalDto, Long, HospitalListItemView> viewProvider() {
        return new HospitalsOverviewViewProvider();
    }

}
