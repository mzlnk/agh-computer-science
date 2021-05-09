package pl.mzlnk.emergencyspotapi.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.HospitalStayDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.HospitalStayDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.NewHospitalStayDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;
import pl.mzlnk.emergencyspotapi.model.params.EntityParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalStayParams;
import pl.mzlnk.emergencyspotapi.repository.HospitalPatientRepository;
import pl.mzlnk.emergencyspotapi.repository.HospitalStayRepository;
import pl.mzlnk.emergencyspotapi.repository.HospitalWardRepository;
import pl.mzlnk.emergencyspotapi.service.HospitalStayService;
import pl.mzlnk.emergencyspotapi.utils.distance.BiOptional;

import javax.persistence.EntityExistsException;
import javax.persistence.EntityNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Represents implementation of service API for hospital stays
 */
@Service
@AllArgsConstructor
public class HospitalStayServiceImpl implements HospitalStayService {

    private final HospitalStayRepository hospitalStayRepository;
    private final HospitalWardRepository hospitalWardRepository;
    private final HospitalPatientRepository hospitalPatientRepository;

    /**
     * Obtain list of hospital stays based on given parameters
     * @param params search parameters
     * @return list of hospital stays
     */
    @Override
    public List<HospitalStayDto> findAll(EntityParams<HospitalStay> params) {
        HospitalStayParams hospitalStayParams = (HospitalStayParams) params;

        return hospitalStayRepository.findHospitalStaysByDateFromAfterAndDateToBefore(hospitalStayParams.dateFrom, hospitalStayParams.dateTo)
                .stream()
                .map(HospitalStayDto::fromEntity)
                .collect(Collectors.toList());
    }

    /**
     * Obtain details about hospital stay with given ID
     * @param id hospital stay's unique ID
     * @return hospital stay details or null if hospital stay with given ID does not exist
     */
    @Override
    public Optional<HospitalStayDetailsDto> findOne(Long id) {
        return hospitalStayRepository.findById(id)
                .map(HospitalStayDetailsDto::fromEntity);
    }

    /**
     * Create new hospital stay
     * @param entity DTO instance representing information about creating hospital stay
     * @return details of created hospital stay
     */
    @Override
    public HospitalStayDetailsDto create(NewHospitalStayDto entity) {
        return BiOptional.of(
                hospitalWardRepository.findById(entity.getHospitalWardId()),
                hospitalPatientRepository.findById(entity.getPatientId())
        )
                .map((hospitalWard, hospitalPatient) -> {
                            HospitalStay hospitalStay = HospitalStay.builder()
                                    .hospitalPatient(hospitalPatient)
                                    .hospitalWard(hospitalWard)
                                    .dateFrom(entity.getDateFrom())
                                    .dateTo(entity.getDateTo())
                                    .build();

                            return hospitalStayRepository.save(hospitalStay);
                        }
                )
                .map(HospitalStayDetailsDto::fromEntity)
                .orElseThrow(EntityNotFoundException::new);
    }

    /**
     * Delete existing object
     * @param id hospital stay's unique ID
     */
    @Override
    public void deleteById(Long id) {
        hospitalStayRepository.deleteById(id);
    }

}
