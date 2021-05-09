package pl.mzlnk.emergencyspot.ui.view.list;

import android.content.Context;
import android.util.AttributeSet;

import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;

public class HospitalListItemView extends BaseListItemView {

    public HospitalListItemView(Context context) {
        super(context);
    }

    public HospitalListItemView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    public void setDetails(HospitalDto hospitalDto) {
        super.setTitle(hospitalDto.getName());
        super.setSubtitle(hospitalDto.getDescription());
        super.setButtonLabel("szczegóły");
    }

}
