package pl.mzlnk.emergencyspotapi.model.params;

import lombok.Builder;
import org.springframework.data.domain.Example;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalPatient;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalStay;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

import java.util.Calendar;
import java.util.GregorianCalendar;

/**
 * Represents implementation of entity params for HospitalStay entity
 */
@Builder
public class HospitalStayParams implements EntityParams<HospitalStay> {

    public final Long hospitalWardId;
    public final Long hospitalPatientId;

    @Builder.Default
    public final Calendar dateFrom = new GregorianCalendar(1980, Calendar.JANUARY, 1);

    @Builder.Default
    public final Calendar dateTo = new GregorianCalendar(2100, Calendar.JANUARY, 1);

    @Override
    public Example<HospitalStay> toExample() {
        return Example.of(
                HospitalStay
                        .builder()
                        .hospitalWard(
                                HospitalWard
                                        .builder()
                                        .id(this.hospitalWardId)
                                        .build()
                        )
                        .hospitalPatient(
                                HospitalPatient
                                        .builder()
                                        .id(this.hospitalPatientId)
                                        .build()
                        )
                        .build()
        );
    }

}
