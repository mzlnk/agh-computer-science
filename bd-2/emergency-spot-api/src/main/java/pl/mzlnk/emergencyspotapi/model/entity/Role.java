package pl.mzlnk.emergencyspotapi.model.entity;

import lombok.Data;

import javax.persistence.AttributeOverride;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Entity class representing structure of roles table in database
 */
@Data
@Entity
@Table(name = "roles")
@AttributeOverride(name = "id", column = @Column(name = "role_id"))
public class Role extends IdentifiableEntity {

    @Column
    private String name;

    @Column
    private String description;

}
