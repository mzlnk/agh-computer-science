package pl.mzlnk.emergencyspotapi.service.impl;

import lombok.AllArgsConstructor;
import org.slf4j.Logger;
import org.springframework.stereotype.Service;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.AddressDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.NewHospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.params.EntityParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalWardParams;
import pl.mzlnk.emergencyspotapi.repository.HospitalRepository;
import pl.mzlnk.emergencyspotapi.repository.HospitalWardRepository;
import pl.mzlnk.emergencyspotapi.service.HospitalService;
import pl.mzlnk.emergencyspotapi.service.HospitalWardService;

import javax.persistence.EntityNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Represents implementation of service API for hospital wards
 */
@Service
@AllArgsConstructor
public class HospitalWardServiceImpl implements HospitalWardService {

    private final HospitalWardRepository hospitalWardRepository;
    private final HospitalRepository hospitalRepository;
    private final Logger logger;

    /**
     * Obtain list of hospital wards based on given parameters
     * @param params search parameters
     * @return list of hospital wards
     */
    @Override
    public List<HospitalWardDto> findAll(EntityParams<HospitalWard> params) {
        return hospitalWardRepository
                .findAll(params.toExample())
                .stream()
                .filter(ward -> ward.getCapacity() >= ((HospitalWardParams) params).minCapacity)
                .filter(ward -> ward.getCapacity() <= ((HospitalWardParams) params).maxCapacity)
                .map(HospitalWardDto::fromEntity)
                .collect(Collectors.toList());
    }

    /**
     * Obtain details about hospital ward with given ID
     * @param id hospital ward's unique ID
     * @return hospital ward details or null if hospital ward with given ID does not exist
     */
    @Override
    public Optional<HospitalWardDetailsDto> findOne(Long id) {
        return hospitalWardRepository
                .findById(id)
                .map(HospitalWardDetailsDto::fromEntity);
    }

    /**
     * Create new hospital ward
     * @param dto DTO instance representing information about creating hospital ward
     * @return details of created hospital ward
     */
    @Override
    public HospitalWardDetailsDto create(NewHospitalWardDto dto) {
        return hospitalRepository.findById(dto.getHospitalId())
                .map(
                        hospital -> {
                            HospitalWard hospitalWard = HospitalWard.builder()
                                    .wardType(dto.getWardType())
                                    .capacity(dto.getCapacity())
                                    .hospital(hospital)
                                    .build();

                            return hospitalWardRepository.save(hospitalWard);
                        }
                )
                .map(HospitalWardDetailsDto::fromEntity)
                .orElseThrow(EntityNotFoundException::new);
    }

    /**
     * Delete existing hospital ward
     * @param id hospital ward's unique ID
     */
    @Override
    public void deleteById(Long id) {
        hospitalWardRepository.deleteById(id);
    }

}
