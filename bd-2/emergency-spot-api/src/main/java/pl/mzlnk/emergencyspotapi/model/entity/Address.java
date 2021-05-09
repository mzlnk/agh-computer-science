package pl.mzlnk.emergencyspotapi.model.entity;

import lombok.*;

import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 * Entity class representing structure of address columns in database
 */
@Data
@Embeddable
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Address {

    @Column(name = "country")
    private String country;

    @Column(name = "city")
    private String city;

    @Column(name = "street")
    private String street;

    @Column(name = "street_number")
    private Long streetNumber;

}
