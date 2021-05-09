package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalreview.HospitalReviewDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.HospitalStayDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.NewHospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;
import pl.mzlnk.emergencyspotapi.model.params.HospitalReviewParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalStayParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalWardParams;
import pl.mzlnk.emergencyspotapi.service.HospitalReviewService;
import pl.mzlnk.emergencyspotapi.service.HospitalStayService;
import pl.mzlnk.emergencyspotapi.service.HospitalWardService;

import java.util.List;
import java.util.Optional;

/**
 * Controller dedicated to handle /wards/* endpoints
 */
@RestController
@RequestMapping("/wards")
@AllArgsConstructor
public class HospitalWardController {

    private final HospitalWardService hospitalWardService;
    private final HospitalReviewService hospitalReviewService;
    private final HospitalStayService hospitalStayService;

    /**
     * Handle GET request to obtain list of hospital ward based on given parameters
     * @param wardType hospital ward type
     * @param minCapacity minimum hospital ward's capacity
     * @param maxCapacity maximum hospital ward's capacity
     * @param hospitalId hospital which hospital ward is associated with
     * @return list of applicable hospital wards
     */
    @GetMapping
    public List<HospitalWardDto> findAll(@RequestParam(required = false, name = "ward") HospitalWardTypeEnum wardType,
                                         @RequestParam(required = false, name = "min_capacity", defaultValue = "0") Integer minCapacity,
                                         @RequestParam(required = false, name = "max_capacity", defaultValue = "2147483647") Integer maxCapacity,
                                         @RequestParam(required = false, name = "hospital") Long hospitalId) {

        return hospitalWardService
                .findAll(
                        HospitalWardParams
                                .builder()
                                .wardType(wardType)
                                .minCapacity(minCapacity)
                                .maxCapacity(maxCapacity)
                                .hospitalId(hospitalId)
                                .build()
                );
    }

    /**
     * Handle GET request to obtain details of hospital ward with given ID
     * @param id hospital ward's unique ID
     * @return DTO instance with hospital ward's details or null if hospital ward with given ID does not exist
     */
    @GetMapping("/{id}")
    public Optional<HospitalWardDetailsDto> findOne(@PathVariable Long id) {
        return hospitalWardService.findOne(id);
    }

    /**
     * Handle GET request to obtain list of hospital reviews associated with given hospital ward
     * @param id hospital ward's unique ID
     * @return list of hospital reviews
     */
    @GetMapping("/{id}/reviews")
    public List<HospitalReviewDto> findHospitalReviews(@PathVariable Long id) {
        return hospitalReviewService
                .findAll(
                        HospitalReviewParams
                                .builder()
                                .hospitalId(id)
                                .build()
                );
    }

    /**
     * Handle GET request to obtain list of hospital stays associated with given hospital ward
     * @param id hospital ward's unique ID
     * @return list of hospital reviews
     */
    @GetMapping("/{id}/stays")
    public List<HospitalStayDto> findHospitalStays(@PathVariable Long id) {
        return hospitalStayService
                .findAll(
                        HospitalStayParams
                                .builder()
                                .hospitalWardId(id)
                                .build()
                );
    }

    /**
     * Handle POST request to create new hospital ward with given details
     * @param hospitalWard details of created hospital ward
     */
    @PostMapping
    public void createHospitalWard(@RequestBody NewHospitalWardDto hospitalWard) {
        hospitalWardService.create(hospitalWard);
    }

    /**
     * Handle DELETE request to delete existing hospital ward
     * @param id hospital ward's unique ID
     */
    @DeleteMapping("/{id}")
    public void deleteHospitalWard(@PathVariable Long id) {
        hospitalWardService.deleteById(id);
    }

}
