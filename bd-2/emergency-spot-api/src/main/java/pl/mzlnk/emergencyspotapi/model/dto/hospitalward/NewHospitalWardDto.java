package pl.mzlnk.emergencyspotapi.model.dto.hospitalward;

import lombok.Data;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

/**
 * DTO class representing hospital ward to be created
 */
@Data
public class NewHospitalWardDto {

    private Long hospitalId;
    private HospitalWardTypeEnum wardType;

    private Long capacity;

}
