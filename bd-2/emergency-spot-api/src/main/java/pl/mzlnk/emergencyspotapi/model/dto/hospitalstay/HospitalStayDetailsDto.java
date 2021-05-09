package pl.mzlnk.emergencyspotapi.model.dto.hospitalstay;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;

import java.util.Calendar;
import java.util.Optional;

/**
 * DTO class representing hospital stay details
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalStayDetailsDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalStay entity
     * @return DTO instance
     */
    public static HospitalStayDetailsDto fromEntity(HospitalStay hospitalStay) {
        HospitalStayDetailsDto dto = new HospitalStayDetailsDto();

        dto.id = hospitalStay.getId();
        dto.ward = HospitalWardDto.fromEntity(hospitalStay.getHospitalWard());
        dto.dateFrom = hospitalStay.getDateFrom();
        dto.dateTo = hospitalStay.getDateTo();
        dto.review = Optional
                .ofNullable(hospitalStay.getHospitalReview())
                .map(HospitalReviewDto::fromEntity)
                .orElse(null);

        return dto;
    }

    private Long id;
    private HospitalWardDto ward;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Calendar dateFrom;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Calendar dateTo;

    private HospitalReviewDto review;

}
