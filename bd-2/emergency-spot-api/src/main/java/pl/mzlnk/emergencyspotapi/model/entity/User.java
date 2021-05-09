package pl.mzlnk.emergencyspotapi.model.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import javax.persistence.*;
import java.util.Set;

/**
 * Entity class representing structure of users table in database
 */
@Data
@Entity
@Table(name = "users")
@AttributeOverride(name = "id", column = @Column(name = "user_id"))
public class User extends IdentifiableEntity {

    @Column
    private String username;

    @Column
    private String password;

    @ManyToMany(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    @JoinTable(
            name = "users_roles",
            joinColumns = {@JoinColumn(name = "user_user_id")},
            inverseJoinColumns = {@JoinColumn(name = "roles_role_id")})
    private Set<Role> roles;

    @OneToOne(cascade = CascadeType.ALL)
    @JoinColumn(name = "hospital_patient_id", referencedColumnName = "hospital_patient_id")
    private HospitalPatient hospitalPatient;

}
