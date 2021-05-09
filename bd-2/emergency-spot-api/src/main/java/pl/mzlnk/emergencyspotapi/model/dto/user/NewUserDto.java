package pl.mzlnk.emergencyspotapi.model.dto.user;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * DTO class representing user to be created
 */
@Data
@AllArgsConstructor
public class NewUserDto {

    private String username;
    private String firstName;
    private String lastName;
    private String pesel;
    private String password;

}
