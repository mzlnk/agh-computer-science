package pl.mzlnk.emergencyspotapi.service;

import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.NewHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.UpdateHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;

/**
 * Represents an detailed API for service associated with hospital reviews
 */
public interface HospitalReviewService extends EntityService<HospitalReviewDto, HospitalReviewDetailsDto, NewHospitalReviewDto, HospitalReview> {

    /**
     * Update existing hospital review
     * @param dto DTO instance representing information of updated hospital review
     * @return details of updated hospital review
     */
    HospitalReviewDetailsDto update(UpdateHospitalReviewDto dto);

}
