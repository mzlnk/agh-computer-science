package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.HospitalStayDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.HospitalStayDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalstay.NewHospitalStayDto;
import pl.mzlnk.emergencyspotapi.model.dto.user.UserDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;
import pl.mzlnk.emergencyspotapi.model.params.HospitalStayParams;
import pl.mzlnk.emergencyspotapi.service.HospitalStayService;
import pl.mzlnk.emergencyspotapi.service.UserService;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Optional;

/**
 * Controller dedicated to handle /stays/* endpoints
 */
@RestController
@RequestMapping("/stays")
@AllArgsConstructor
public class HospitalStayController {

    /**
     * Injected HospitalStayService instance
     */
    private final HospitalStayService hospitalStayService;

    /**
     * Injected UserService instance
     */
    private final UserService userService;

    /**
     * Handle GET request to obtain list of hospital stays based on given parameters
     * @param dateFrom start date range
     * @param dateTo end date range
     * @param hospitalWardId - hospital ward's ID where hospital stay is associated with
     * @param hospitalPatientId - hospital patient's ID where hospital stay is associated with
     * @return list of hospital stays
     */
    @GetMapping
    public List<HospitalStayDto> findAll(@RequestParam(required = false, name = "date_from") @DateTimeFormat(pattern = "dd-MM-yyyy") Calendar dateFrom,
                                         @RequestParam(required = false, name = "date_to") @DateTimeFormat(pattern = "dd-MM-yyyy") Calendar dateTo,
                                         @RequestParam(required = false, name = "ward") Long hospitalWardId,
                                         @RequestParam(required = false, name = "patient") Long hospitalPatientId) {
        return hospitalStayService
                .findAll(
                        HospitalStayParams
                                .builder()
                                .dateFrom(dateFrom)
                                .dateTo(dateTo)
                                .hospitalWardId(hospitalWardId)
                                .hospitalPatientId(hospitalPatientId)
                                .build()
                );
    }

    /**
     * Handle GET request to obtain details of hospital stay with given ID
     * @param id hospital stay's unique ID
     * @return hospital stay details or null if hospital stay with given ID does not exist
     */
    @GetMapping("/{id}")
    public Optional<HospitalStayDetailsDto> findById(@PathVariable Long id) {
        return hospitalStayService.findOne(id);
    }

    /**
     * Handle GET request to obtains users hospital stays
     * @param principal instance containing user authentication data (gained from JWT token)
     * @return list of user's hospital stays
     */
    @GetMapping("/me")
    public List<HospitalStayDto> findUserHospitalStays(Principal principal) {
        return userService
                .findByUsername(principal.getName())
                .map(UserDto::getId)
                .map(id -> {
                    return hospitalStayService
                            .findAll(
                                    HospitalStayParams
                                            .builder()
                                            .hospitalPatientId(id)
                                            .build()
                            );
                })
                .orElse(new ArrayList<>());
    }

    /**
     * Handle POST request to create new hospital stay
     * @param hospitalStay details of new hospital stay
     * @return DTO instance with created hospital stay details
     */
    @PostMapping
    public HospitalStayDetailsDto createHospitalStay(@RequestBody NewHospitalStayDto hospitalStay) {
        return hospitalStayService.create(hospitalStay);
    }

    /**
     * Handle DELETE request to delete existing hospital stay
     * @param id hospital stay's ID which has to be removed
     */
    @DeleteMapping("/{id}")
    public void deleteHospitalStay(@PathVariable Long id) {
        hospitalStayService.deleteById(id);
    }

}
