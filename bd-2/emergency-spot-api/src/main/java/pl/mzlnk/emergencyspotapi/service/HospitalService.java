package pl.mzlnk.emergencyspotapi.service;

import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.HospitalDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospital.NewHospitalDto;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;

import java.util.Optional;

/**
 * Represents an detailed API for service associated with hospitals
 */
public interface HospitalService extends EntityService<HospitalDto, HospitalDetailsDto, NewHospitalDto, Hospital> {

    /**
     * Obtain nearest hospital based on given location details
     * @param longitude given location longitude
     * @param latitude given location latitude
     * @return nearest hospital to given location details or null if no hospital exists
     */
    Optional<HospitalDetailsDto> findNearest(double longitude, double latitude);

}
