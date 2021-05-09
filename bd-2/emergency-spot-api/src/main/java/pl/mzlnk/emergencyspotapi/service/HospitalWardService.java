package pl.mzlnk.emergencyspotapi.service;

import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDetailsDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.dto.hospitalward.NewHospitalWardDto;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

/**
 * Represents an detailed API for service associated with hospital wards
 */
public interface HospitalWardService extends EntityService<HospitalWardDto, HospitalWardDetailsDto, NewHospitalWardDto, HospitalWard> {

}
