package pl.mzlnk.emergencyspot.model.hospitalward;

import lombok.Data;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;

@Data
public class HospitalWardDetailsDto {

    private HospitalDto hospital;
    private HospitalWardEnum wardType;

    private Long capacity;
    private Long currentPatients;

    private Double averageRating;

}
