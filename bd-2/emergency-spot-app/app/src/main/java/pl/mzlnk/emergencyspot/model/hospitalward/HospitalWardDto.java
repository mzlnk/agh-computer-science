package pl.mzlnk.emergencyspot.model.hospitalward;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;

@Data
public class HospitalWardDto {

    private HospitalDto hospital;
    private HospitalWardEnum wardType;

}
