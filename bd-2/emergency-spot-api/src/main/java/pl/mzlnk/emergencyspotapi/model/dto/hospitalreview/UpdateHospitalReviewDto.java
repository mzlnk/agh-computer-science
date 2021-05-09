package pl.mzlnk.emergencyspotapi.model.dto.hospitalreview;

import lombok.Data;

/**
 * DTO class representing hospital review to be updated
 */
@Data
public class UpdateHospitalReviewDto {

    private Long hospitalReviewId;
    private Double newRating;

}
