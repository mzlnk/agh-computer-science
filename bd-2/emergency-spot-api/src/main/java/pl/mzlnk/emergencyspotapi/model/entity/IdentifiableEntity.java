package pl.mzlnk.emergencyspotapi.model.entity;

import lombok.Getter;
import lombok.experimental.SuperBuilder;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;

/**
 * Entity superclass describing identification of rows in tables in database
 */
@Getter
@SuperBuilder(toBuilder = true)
@MappedSuperclass
public class IdentifiableEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    public IdentifiableEntity() {

    }

}
