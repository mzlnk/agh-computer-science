package pl.mzlnk.emergencyspot.ui.view.list;

import android.content.Context;
import android.util.AttributeSet;

import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;

public class HospitalWardListItemView extends BaseListItemView {

    public HospitalWardListItemView(Context context) {
        super(context);
    }

    public HospitalWardListItemView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    public void setDetails(HospitalWardDto hospitalWardDto) {
        super.setTitle(hospitalWardDto.getWardType().name());
        super.setSubtitle("");
        super.setButtonLabel("szczegóły");
    }

}
