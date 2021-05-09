package pl.mzlnk.emergencyspot.ui.asynctask;

import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalWardListItemView;
import pl.mzlnk.emergencyspot.ui.viewprovider.HospitalWardsOverviewViewProvider;
import pl.mzlnk.emergencyspot.ui.viewprovider.ViewProvider;

public class HospitalWardsOverviewLoadViewsTask extends BaseLoadViewsTask<HospitalWardDto, Long, HospitalWardListItemView> {

    public HospitalWardsOverviewLoadViewsTask(LoadViewsTaskReceiver<HospitalWardDto, Long, HospitalWardListItemView> receiver) {
        super(receiver);
    }

    @Override
    protected ViewProvider<HospitalWardDto, Long, HospitalWardListItemView> viewProvider() {
        return new HospitalWardsOverviewViewProvider();
    }

}
