package pl.mzlnk.emergencyspotapi.model.dto.hospital;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;

/**
 * DTO class representing hospital
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalDto {

    /**
     * Obtain instance based on entity representation
     * @param hospital entity
     * @return DTO instance
     */
    public static HospitalDto fromEntity(Hospital hospital) {
        HospitalDto dto = new HospitalDto();

        dto.id = hospital.getId();
        dto.longitude = hospital.getLongitude();
        dto.latitude = hospital.getLatitude();
        dto.name = hospital.getName();
        dto.description = hospital.getDescription();
        dto.address = AddressDto.fromEntity(hospital.getAddress());

        return dto;
    }

    private Long id;
    private Double longitude;
    private Double latitude;
    private String name;
    private String description;
    private AddressDto address;

}
