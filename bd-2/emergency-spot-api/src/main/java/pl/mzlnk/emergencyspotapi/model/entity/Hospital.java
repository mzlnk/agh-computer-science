package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonBackReference;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import javax.persistence.*;
import java.util.List;

/**
 * Entity class representing structure of hospitals table in database
 */
@Data
@Entity
@SuperBuilder
@AllArgsConstructor
@AttributeOverride(name = "id", column = @Column(name = "hospital_id"))
@Table(name = "hospitals")
public class Hospital extends IdentifiableEntity {

    @Column(name = "name")
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "longitude")
    private Double longitude;

    @Column(name = "latitude")
    private Double latitude;

    @Embedded
    private Address address;

    @OneToMany(mappedBy = "hospital")
    @JsonBackReference
    private List<HospitalWard> wards;

    public Hospital() {
        super();
    }

    public Hospital(String name, String description, Double longitude, Double latitude, Address address) {
        this(name, description, longitude, latitude, address, null);
    }

}
