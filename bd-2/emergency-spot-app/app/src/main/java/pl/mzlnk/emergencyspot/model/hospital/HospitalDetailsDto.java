package pl.mzlnk.emergencyspot.model.hospital;

import java.util.List;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;

@Data
public class HospitalDetailsDto {

    private String name;
    private String description;
    private AddressDto address;
    private Double averageRating;
    private List<HospitalWardDto> wards;

}
