package pl.mzlnk.emergencyspotapi.model.dto.hospital;

import lombok.AccessLevel;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

import java.util.List;
import java.util.stream.Collectors;

/**
 * DTO class representing hospital details
 */
@Data
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class HospitalDetailsDto {

    /**
     * Obtain instance based on entity representation
     * @param hospital entity
     * @return DTO instance
     */
    public static HospitalDetailsDto fromEntity(Hospital hospital) {
        HospitalDetailsDto dto = new HospitalDetailsDto();

        dto.id = hospital.getId();
        dto.name = hospital.getName();
        dto.description = hospital.getDescription();
        dto.longitude = hospital.getLongitude();
        dto.latitude = hospital.getLatitude();
        dto.address = AddressDto.fromEntity(hospital.getAddress());

        dto.averageRating = hospital.getWards()
                .stream()
                .map(HospitalWard::getAverageRating)
                .mapToDouble(rating -> rating)
                .average()
                .orElse(0);

        dto.wards = hospital.getWards()
                .stream()
                .map(HospitalWardDto::fromEntity)
                .collect(Collectors.toList());

        return dto;
    }

    private Long id;
    private String name;
    private String description;
    private Double longitude;
    private Double latitude;
    private AddressDto address;
    private Double averageRating;
    private List<HospitalWardDto> wards;

}
