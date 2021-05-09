package pl.mzlnk.emergencyspotapi.model.dto.hospitalreview;

import lombok.Data;

/**
 * DTO class representing hospital review to be created
 */
@Data
public class NewHospitalReviewDto {

    private Long hospitalStayId;
    private Double rating;

}
