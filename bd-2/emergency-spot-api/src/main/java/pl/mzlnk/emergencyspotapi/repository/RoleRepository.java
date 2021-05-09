package pl.mzlnk.emergencyspotapi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import pl.mzlnk.emergencyspotapi.model.entity.Role;

/**
 * Represents an API for access to data stored in roles table in database
 */
@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {


}
