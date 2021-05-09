package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

/**
 * Represents an API for access to data stored in hospital_wards table in database
 */
@Repository
public interface HospitalWardRepository extends JpaRepository<HospitalWard, Long> {

}
