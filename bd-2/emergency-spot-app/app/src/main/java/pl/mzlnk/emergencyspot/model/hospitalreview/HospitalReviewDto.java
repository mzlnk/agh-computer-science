package pl.mzlnk.emergencyspot.model.hospitalreview;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;

@Data
public class HospitalReviewDto {

    private HospitalWardDto ward;
    private Double rating;

}
