package pl.mzlnk.emergencyspotapi.model.dto.user;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.entity.User;

/**
 * DTO class representing user
 */
@Data
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class UserDto {

    /**
     * Obtain instance based on entity representation
     * @param user entity
     * @return DTO instance
     */
    public static UserDto fromEntity(User user) {
        UserDto dto = new UserDto();

        dto.id = user.getHospitalPatient().getId();
        dto.username = user.getUsername();
        dto.password = user.getPassword();

        dto.firstName = user.getHospitalPatient().getFirstName();
        dto.lastName = user.getHospitalPatient().getLastName();
        dto.pesel = user.getHospitalPatient().getPesel();

        return dto;
    }

    private Long id;
    private String firstName;
    private String lastName;
    private String pesel;

    private String username;

    // todo: set access property
    private String password;

}
