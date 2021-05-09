package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;
import java.util.List;

/**
 * Entity class representing structure of hospital_patients table in database
 */
@Getter
@Entity
@SuperBuilder
@AllArgsConstructor
@Table(name = "hospital_patients")
@AttributeOverride(name = "id", column = @Column(name = "hospital_patient_id"))
public class HospitalPatient extends IdentifiableEntity {

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "pesel")
    private String pesel;

    @OneToOne(mappedBy = "hospitalPatient")
    private User user;

    @OneToMany(mappedBy = "hospitalPatient")
    @JsonBackReference
    private List<HospitalStay> hospitalStays;

    public HospitalPatient() {
        super();
    }

    public HospitalPatient(String firstName, String lastName, String pesel) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.pesel = pesel;
    }

}
