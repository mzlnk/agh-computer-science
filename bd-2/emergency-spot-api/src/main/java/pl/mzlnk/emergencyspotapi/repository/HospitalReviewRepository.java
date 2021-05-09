package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;

/**
 * Represents an API for access to data stored in hospital_reviews table in database
 */
@Repository
public interface HospitalReviewRepository extends JpaRepository<HospitalReview, Long> {

}
