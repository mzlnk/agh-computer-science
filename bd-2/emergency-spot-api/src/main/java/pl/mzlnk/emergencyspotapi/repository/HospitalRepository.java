package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;

/**
 * Represents an API for access to data stored in hospitals table in database
 */
@Repository
public interface HospitalRepository extends JpaRepository<Hospital, Long> {

}
