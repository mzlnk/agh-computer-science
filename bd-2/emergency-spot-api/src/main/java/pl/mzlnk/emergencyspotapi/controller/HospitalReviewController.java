package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.NewHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.UpdateHospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;
import pl.mzlnk.emergencyspotapi.model.params.HospitalReviewParams;
import pl.mzlnk.emergencyspotapi.service.HospitalReviewService;

import java.util.List;
import java.util.Optional;

/**
 * Controller dedicated to handle /reviews/* endpoints
 */
@RestController
@RequestMapping("/reviews")
@AllArgsConstructor
public class HospitalReviewController {

    /**
     * Injected HospitalReviewService instance
     */
    private final HospitalReviewService hospitalReviewService;

    /**
     * Handle GET request to obtain hospital reviews based on given parameters
     * @param minRating minimum review rating
     * @param maxRating maximum review rating
     * @param hospitalId reviews connected to hospital with given ID
     * @return list of hospital reviews
     */
    @GetMapping
    public List<HospitalReviewDto> findAll(@RequestParam(required = false, name = "min_rating") Double minRating,
                                           @RequestParam(required = false, name = "max_rating") Double maxRating,
                                           @RequestParam(required = false, name = "hospital") Long hospitalId) {

        return hospitalReviewService
                .findAll(
                        HospitalReviewParams
                                .builder()
                                .minRating(minRating)
                                .maxRating(maxRating)
                                .hospitalId(hospitalId)
                                .build()
                );
    }

    /**
     * Handle GET request to obtain details of hospital review with given ID
     * @param id given hospital review's unique ID
     * @return details of hospital review
     */
    @GetMapping("/{id}")
    public Optional<HospitalReviewDetailsDto> findOne(@PathVariable Long id) {
        return hospitalReviewService.findOne(id);
    }

    /**
     * Handle POST request to create new review
     * @param review review to be created
     * @return DTO instance with created review details
     */
    @PostMapping
    public HospitalReviewDetailsDto createReview(@RequestBody NewHospitalReviewDto review) {
        return hospitalReviewService.create(review);
    }

    /**
     * Handle PUT request to update existing review
     * @param review details of review to be updated
     * @return DTO instance with updated review details
     */
    @PutMapping
    public HospitalReviewDetailsDto updateReview(@RequestBody UpdateHospitalReviewDto review) {
        return hospitalReviewService.update(review);
    }

    /**
     * Handle DELETE request to delete existing review
     * @param id hospital review's ID which has to be removed
     */
    @DeleteMapping("/{id}")
    public void deleteReview(@PathVariable Long id) {
        hospitalReviewService.deleteById(id);
    }

}
