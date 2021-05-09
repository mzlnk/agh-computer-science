package pl.mzlnk.emergencyspotapi.model.dto.hospitalward;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

/**
 * DTO class representing hospital ward
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalWardDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalWard entity
     * @return DTO instance
     */
    public static HospitalWardDto fromEntity(HospitalWard hospitalWard) {
        HospitalWardDto dto = new HospitalWardDto();

        dto.id = hospitalWard.getId();
        dto.hospital = HospitalDto.fromEntity(hospitalWard.getHospital());
        dto.wardType = hospitalWard.getWardType();

        return dto;
    }

    private Long id;
    private HospitalDto hospital;
    private HospitalWardTypeEnum wardType;

}
