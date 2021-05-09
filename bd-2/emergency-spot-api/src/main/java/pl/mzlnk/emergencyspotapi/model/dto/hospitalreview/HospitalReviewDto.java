package pl.mzlnk.emergencyspotapi.model.dto.hospitalreview;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

/**
 * DTO class representing hospital review
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalReviewDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalReview entity
     * @return DTO instance
     */
    public static HospitalReviewDto fromEntity(HospitalReview hospitalReview) {
        HospitalReviewDto dto = new HospitalReviewDto();

        dto.id = hospitalReview.getId();
        dto.rating = hospitalReview.getRating();

        return dto;
    }

    private Long id;
    private Double rating;

}
