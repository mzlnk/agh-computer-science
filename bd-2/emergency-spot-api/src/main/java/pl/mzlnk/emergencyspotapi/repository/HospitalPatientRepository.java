package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalPatient;

/**
 * Represents an API for access to data stored in hospital_patients table in database
 */
@Repository
public interface HospitalPatientRepository extends JpaRepository<HospitalPatient, Long> {


}
