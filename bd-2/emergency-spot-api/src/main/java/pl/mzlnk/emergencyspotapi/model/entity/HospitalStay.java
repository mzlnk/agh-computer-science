package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;
import java.util.Calendar;

/**
 * Entity class representing structure of hospital_stays table in database
 */
@Getter
@Entity
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@Table(name = "hospital_stays")
@AttributeOverride(name = "id", column = @Column(name = "hospital_stay_id"))
public class HospitalStay extends IdentifiableEntity {

    @Column(name = "date_from")
    @Temporal(TemporalType.DATE)
    @JsonFormat(pattern = "dd-MM-yyyy")
    private Calendar dateFrom;

    @Column(name = "date_to")
    @Temporal(TemporalType.DATE)
    @JsonFormat(pattern = "dd-MM-yyyy")
    private Calendar dateTo;

    @OneToOne(mappedBy = "hospitalStay")
    @JsonBackReference
    private HospitalReview hospitalReview;

    @ManyToOne
    @JoinColumn(name = "hospital_patient_id", referencedColumnName = "hospital_patient_id")
    @JsonBackReference
    private HospitalPatient hospitalPatient;

    @ManyToOne
    @JoinColumn(name = "hospital_ward_id", referencedColumnName = "hospital_ward_id")
    @JsonBackReference
    private HospitalWard hospitalWard;

    public HospitalStay() {
        super();
    }

}
