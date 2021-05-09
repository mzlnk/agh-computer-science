package pl.mzlnk.emergencyspotapi.model.dto.hospitalstay;

import java.util.Calendar;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;

/**
 * DTO class representing hospital stay
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalStayDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalStay entity
     * @return DTO instance
     */
    public static HospitalStayDto fromEntity(HospitalStay hospitalStay) {
        HospitalStayDto dto = new HospitalStayDto();

        dto.id = hospitalStay.getId();
        dto.ward = HospitalWardDto.fromEntity(hospitalStay.getHospitalWard());
        dto.dateFrom = hospitalStay.getDateFrom();
        dto.dateTo = hospitalStay.getDateTo();

        return dto;
    }

    private Long id;
    private HospitalWardDto ward;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Calendar dateFrom;

    @JsonFormat(pattern = "yyyy-MM-dd")
    private Calendar dateTo;

}
