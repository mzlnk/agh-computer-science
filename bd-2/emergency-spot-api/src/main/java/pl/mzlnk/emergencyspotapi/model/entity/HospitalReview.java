package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;

/**
 * Entity class representing structure of hospital_reviews table in database
 */
@Entity
@Data
@SuperBuilder
@AllArgsConstructor
@Table(name = "hospital_reviews")
@AttributeOverride(name = "id", column = @Column(name = "hospital_review_id"))
public class HospitalReview extends IdentifiableEntity {

    @Column(name = "rating")
    private Double rating;

    @ManyToOne
    @JoinColumn(name = "hospital_ward_id", referencedColumnName = "hospital_ward_id")
    @JsonBackReference
    private HospitalWard hospitalWard;

    @OneToOne
    @JoinColumn(name = "hospital_stay_id", referencedColumnName = "hospital_stay_id")
    @JsonBackReference
    private HospitalStay hospitalStay;

    public HospitalReview() {
        super();
    }

}
