package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;
import pl.mzlnk.emergencyspotapi.model.params.HospitalParams;
import pl.mzlnk.emergencyspotapi.model.params.HospitalWardParams;
import pl.mzlnk.emergencyspotapi.service.HospitalService;
import pl.mzlnk.emergencyspotapi.service.HospitalWardService;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Controller dedicated to handle /hospitals/* endpoints
 */
@RestController
@RequestMapping("/hospitals")
@AllArgsConstructor
public class HospitalController {

    /**
     * Injected HospitalService instance
     */
    private final HospitalService hospitalService;

    /**
     * Injected HospitalWardService instance
     */
    private final HospitalWardService hospitalWardService;

    /**
     * Handle GET request to obtain hospitals data based on given parameters
     * @param name hospital name
     * @param longitude hospital longitude
     * @param latitude hospital latitude
     * @param country country where hospital is located
     * @param city city where hospital is located
     * @param wards wards which hospital consists of
     * @return list of hospitals
     */
    @GetMapping
    public List<HospitalDto> findAll(@RequestParam(required = false) String name,
                                     @RequestParam(required = false) Double longitude,
                                     @RequestParam(required = false) Double latitude,
                                     @RequestParam(required = false) String country,
                                     @RequestParam(required = false) String city,
                                     @RequestParam(required = false) List<HospitalWardTypeEnum> wards) {


        return hospitalService
                .findAll(
                        HospitalParams
                                .builder()
                                .name(name)
                                .longitude(longitude)
                                .latitude(latitude)
                                .country(country)
                                .city(city)
                                .wards(wards != null ? wards : new ArrayList<>())
                                .build()
                );
    }

    /**
     * Handle GET request to obtains details about hospital with given ID
     * @param id hospital's unique ID
     * @return hospital details or null if hospital with given ID does not exist
     */
    @GetMapping("/{id}")
    public Optional<HospitalDetailsDto> findById(@PathVariable long id) {
        return hospitalService.findOne(id);
    }

    /**
     * Handle GET reuqest to obtain wards' information of hospital with given ID
     * @param id hospital's unique ID
     * @return list of hospital wards
     */
    @GetMapping("/{id}/wards")
    public List<HospitalWardDto> findHospitalWards(@PathVariable long id) {
        return hospitalWardService
                .findAll(
                        HospitalWardParams
                                .builder()
                                .hospitalId(id)
                                .build()
                );
    }

    /**
     * Handle GET request to obtain nearest hospital based on given location details
     * @param longitude given location longitude
     * @param latitude given location latitude
     * @return nearest hospital to given location details or null if no hospital exists
     */
    @GetMapping("/nearest")
    public Optional<HospitalDetailsDto> findNearest(@RequestParam Double longitude,
                                                    @RequestParam Double latitude) {
        return hospitalService.findNearest(longitude, latitude);
    }

}
