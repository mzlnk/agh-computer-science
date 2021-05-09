package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;

import java.util.Calendar;
import java.util.List;

/**
 * Represents an API for access to data stored in hospital_stays table in database
 */
@Repository
public interface HospitalStayRepository extends JpaRepository<HospitalStay, Long> {

    List<HospitalStay> findHospitalStaysByDateFromAfterAndDateToBefore(Calendar dateFrom, Calendar dateTo);

    @Query("SELECT s FROM HospitalStay s WHERE s.dateFrom <= :date AND s.dateTo >= :date")
    List<HospitalStay> findHospitalStaysByDate(@Param("date") Calendar date);

}
