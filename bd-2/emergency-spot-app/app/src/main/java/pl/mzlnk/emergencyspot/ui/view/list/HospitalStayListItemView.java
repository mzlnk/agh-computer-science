package pl.mzlnk.emergencyspot.ui.view.list;

import android.content.Context;
import android.util.AttributeSet;

import java.text.SimpleDateFormat;

import pl.mzlnk.emergencyspot.model.hospitalstay.HospitalStayDto;

public class HospitalStayListItemView extends BaseListItemView {

    public HospitalStayListItemView(Context context) {
        super(context);
    }

    public HospitalStayListItemView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    public void setDetails(HospitalStayDto hospitalStayDto) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

        String from = sdf.format(hospitalStayDto.getDateFrom().getTime());
        String to = sdf.format(hospitalStayDto.getDateTo().getTime());

        super.setTitle(hospitalStayDto.getWard().getHospital().getName());
        super.setSubtitle(from + " - " + to);
        super.setButtonLabel("szczegóły");
    }

}
