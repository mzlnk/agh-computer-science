package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;
import java.util.Calendar;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Entity class representing structure of hospital_wards table in database
 */
@Data
@Entity
@SuperBuilder
@AllArgsConstructor
@Table(name = "hospital_wards")
@AttributeOverride(name = "id", column = @Column(name = "hospital_ward_id"))
public class HospitalWard extends IdentifiableEntity {

    @Column(name = "ward_type")
    @Enumerated(EnumType.STRING)
    private HospitalWardTypeEnum wardType;

    @Column(name = "capacity")
    private Long capacity;

    @ManyToOne
    @JsonBackReference
    private Hospital hospital;

    @OneToMany(mappedBy = "hospitalWard")
    @JsonBackReference
    private List<HospitalReview> hospitalReviews;

    @OneToMany(mappedBy = "hospitalWard")
    @JsonBackReference
    private List<HospitalStay> hospitalStays;

    public HospitalWard() {
        super();
    }

    public List<HospitalStay> getCurrentHospitalStays() {
        final Calendar now = Calendar.getInstance();

        return hospitalStays
                .stream()
                .filter(stay -> now.after(stay.getDateFrom()) && now.before(stay.getDateTo()))
                .collect(Collectors.toList());
    }

    public double getAverageRating() {
        return hospitalReviews
                .stream()
                .map(HospitalReview::getRating)
                .mapToDouble(rating -> rating)
                .average()
                .orElse(0);
    }

}
