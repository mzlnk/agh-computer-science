package pl.mzlnk.emergencyspotapi.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.NewHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.UpdateHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;
import pl.mzlnk.emergencyspotapi.model.params.EntityParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalReviewParams;
import pl.mzlnk.emergencyspotapi.repository.HospitalReviewRepository;
import pl.mzlnk.emergencyspotapi.repository.HospitalStayRepository;
import pl.mzlnk.emergencyspotapi.service.HospitalReviewService;

import javax.persistence.EntityExistsException;
import javax.persistence.EntityNotFoundException;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Represents implementation of service API for hospital reviews
 */
@Service
@AllArgsConstructor
public class HospitalReviewServiceImpl implements HospitalReviewService {

    private final HospitalReviewRepository hospitalReviewRepository;
    private final HospitalStayRepository hospitalStayRepository;

    /**
     * Obtain list of hospital reviews based on given parameters
     * @param params search parameters
     * @return list of hospital reviews
     */
    @Override
    public List<HospitalReviewDto> findAll(EntityParams<HospitalReview> params) {
        return hospitalReviewRepository
                .findAll(params.toExample())
                .stream()
                .filter(review -> review.getRating() > ((HospitalReviewParams) params).minRating)
                .filter(review -> review.getRating() < ((HospitalReviewParams) params).maxRating)
                .map(HospitalReviewDto::fromEntity)
                .collect(Collectors.toList());
    }

    /**
     * Obtain details about hospital review with given ID
     * @param id hospital review's unique ID
     * @return hospital review details or null if hospital review with given ID does not exist
     */
    @Override
    public Optional<HospitalReviewDetailsDto> findOne(Long id) {
        return hospitalReviewRepository.findById(id)
                .map(HospitalReviewDetailsDto::fromEntity);
    }

    /**
     * Create new hospital review
     * @param entity DTO instance representing information about creating hospital review
     * @return details of created hospital review
     */
    @Override
    public HospitalReviewDetailsDto create(NewHospitalReviewDto entity) {
        return hospitalStayRepository
                .findById(entity.getHospitalStayId())
                .map(hospitalStay -> {
                            HospitalReview hospitalReview = HospitalReview.builder()
                                    .hospitalStay(hospitalStay)
                                    .hospitalWard(hospitalStay.getHospitalWard())
                                    .rating(entity.getRating())
                                    .build();

                            return hospitalReviewRepository.save(hospitalReview);
                        }
                )
                .map(HospitalReviewDetailsDto::fromEntity)
                .orElseThrow(EntityExistsException::new);
    }

    /**
     * Update existing hospital review
     * @param dto DTO instance representing information of updated hospital review
     * @return details of updated hospital review
     */
    @Override
    public HospitalReviewDetailsDto update(UpdateHospitalReviewDto dto) {
        return hospitalReviewRepository
                .findById(dto.getHospitalReviewId())
                .map(hospitalReview -> {
                            hospitalReview.setRating(dto.getNewRating());
                            return hospitalReviewRepository.save(hospitalReview);
                        }
                )
                .map(HospitalReviewDetailsDto::fromEntity)
                .orElseThrow(EntityNotFoundException::new);
    }

    /**
     * Delete existing hospital review
     * @param id hospital review's unique ID
     */
    @Override
    public void deleteById(Long id) {
        hospitalReviewRepository.deleteById(id);
    }

}
