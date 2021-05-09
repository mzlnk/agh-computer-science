package pl.mzlnk.emergencyspotapi.model.dto.hospitalreview;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;

/**
 * DTO class representing hospital review details
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalReviewDetailsDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalReview entity
     * @return DTO instance
     */
    public static HospitalReviewDetailsDto fromEntity(HospitalReview hospitalReview) {
        HospitalReviewDetailsDto dto = new HospitalReviewDetailsDto();

        dto.id = hospitalReview.getId();
        dto.rating = hospitalReview.getRating();
        dto.ward = HospitalWardDto.fromEntity(hospitalReview.getHospitalWard());

        return dto;
    }

    private Long id;
    private Double rating;
    private HospitalWardDto ward;

}
