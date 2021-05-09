package pl.mzlnk.emergencyspotapi.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.AddressDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.NewHospitalDto;
import pl.mzlnk.emergencyspotapi.model.entity.Address;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.params.EntityParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalParams;
import pl.mzlnk.emergencyspotapi.repository.HospitalRepository;
import pl.mzlnk.emergencyspotapi.service.HospitalService;
import pl.mzlnk.emergencyspotapi.utils.distance.Coordinates;
import pl.mzlnk.emergencyspotapi.utils.distance.DistanceUtils;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * Represents implementation of service API for hospitals
 */
@Service
@AllArgsConstructor
public class HospitalServiceImpl implements HospitalService {

    private final HospitalRepository hospitalRepository;
    private final DistanceUtils distanceUtils;

    /**
     * Obtain list of hospitals based on given parameters
     * @param params search parameters
     * @return list of hospitals
     */
    @Override
    public List<HospitalDto> findAll(EntityParams<Hospital> params) {
        List<Hospital> result = hospitalRepository.findAll(params.toExample());

        HospitalParams hospitalParams = (HospitalParams) params;
        if (!hospitalParams.wards.isEmpty()) {
            result = result
                    .stream()
                    .filter(hospital ->
                            hospital.getWards()
                                    .stream()
                                    .map(HospitalWard::getWardType)
                                    .anyMatch(hospitalParams.wards::contains))
                    .collect(Collectors.toList());
        }

        return result
                .stream()
                .map(HospitalDto::fromEntity)
                .collect(Collectors.toList());
    }

    /**
     * Obtain details about hospital with given ID
     * @param id hospital's unique ID
     * @return hospital details or null if hospital with given ID does not exist
     */
    @Override
    public Optional<HospitalDetailsDto> findOne(Long id) {
        return hospitalRepository
                .findById(id)
                .map(HospitalDetailsDto::fromEntity);
    }

    /**
     * Obtain nearest hospital based on given location details
     * @param longitude given location longitude
     * @param latitude given location latitude
     * @return nearest hospital to given location details or null if no hospital exists
     */
    @Override
    public Optional<HospitalDetailsDto> findNearest(double longitude, double latitude) {
        Coordinates coords = new Coordinates(longitude, latitude);

        return hospitalRepository.findAll()
                .parallelStream()
                .map(hospital -> {
                    return new AbstractMap.SimpleEntry<>(
                            hospital,
                            distanceUtils.calculateDistance(coords, Coordinates.fromHospital(hospital))
                    );
                })
                .min(Comparator.comparingDouble(AbstractMap.SimpleEntry::getValue))
                .map(Map.Entry::getKey)
                .map(HospitalDetailsDto::fromEntity);
    }

    /**
     * Create new hospital
     * @param entity DTO instance representing information about creating hospital
     * @return details of created hospital
     */
    @Override
    public HospitalDetailsDto create(NewHospitalDto entity) {
        Address address = Address.builder()
                .country(entity.getAddress().getCountry())
                .city(entity.getAddress().getCity())
                .street(entity.getAddress().getStreet())
                .streetNumber(entity.getAddress().getStreetNumber())
                .build();

        Hospital hospital = Hospital.builder()
                .longitude(entity.getLongitude())
                .latitude(entity.getLatitude())
                .name(entity.getName())
                .description(entity.getDescription())
                .address(address)
                .build();

        Hospital saved = this.hospitalRepository.save(hospital);
        return HospitalDetailsDto.fromEntity(saved);
    }

    /**
     * Delete existing hospital
     * @param id hospital's unique ID
     */
    @Override
    public void deleteById(Long id) {
        hospitalRepository.deleteById(id);
    }

}
