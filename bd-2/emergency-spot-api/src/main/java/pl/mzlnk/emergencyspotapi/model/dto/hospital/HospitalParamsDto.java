package pl.mzlnk.emergencyspotapi.model.dto.hospital;

import lombok.Data;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

import java.util.List;

/**
 * DTO class representing hospital search parameters
 */
@Data
public class HospitalParamsDto {

    private String name;
    private Double longitude;
    private Double latitude;
    private String country;
    private String city;
    private List<HospitalWardTypeEnum> wards;

}
