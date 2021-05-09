package pl.mzlnk.emergencyspot.model.hospital;

import java.util.List;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardEnum;

@Data
public class HospitalParamsDto {

    private String name;
    private Double longitude;
    private Double latitude;
    private String country;
    private String city;
    private List<HospitalWardEnum> wards;

}
