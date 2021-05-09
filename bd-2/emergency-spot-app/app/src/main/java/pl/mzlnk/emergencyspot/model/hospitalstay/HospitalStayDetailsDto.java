package pl.mzlnk.emergencyspot.model.hospitalstay;

import java.util.Calendar;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;

@Data
public class HospitalStayDetailsDto {

    private HospitalWardDto ward;

    private Calendar dateFrom;
    private Calendar dateTo;

    private HospitalReviewDto review;

}
