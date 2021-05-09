package pl.mzlnk.emergencyspotapi.model.dto.hospitalward;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

/**
 * DTO class representing hospital ward details
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalWardDetailsDto {

    /**
     * Obtain instance based on entity representation
     * @param hospitalWard entity
     * @return DTO instance
     */
    public static HospitalWardDetailsDto fromEntity(HospitalWard hospitalWard) {
        HospitalWardDetailsDto dto = new HospitalWardDetailsDto();

        dto.id = hospitalWard.getId();
        dto.hospital = HospitalDto.fromEntity(hospitalWard.getHospital());
        dto.wardType = hospitalWard.getWardType();

        dto.capacity = hospitalWard.getCapacity();
        dto.currentHospitalStays = (long) hospitalWard.getCurrentHospitalStays().size();
        dto.averageRating = hospitalWard.getAverageRating();

        return dto;
    }

    private Long id;
    private HospitalDto hospital;
    private HospitalWardTypeEnum wardType;

    private Long capacity;
    private Long currentHospitalStays;

    private Double averageRating;

}
