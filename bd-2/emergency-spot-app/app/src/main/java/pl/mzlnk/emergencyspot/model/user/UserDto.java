package pl.mzlnk.emergencyspot.model.user;

import lombok.Data;

@Data
public class UserDto {

    private String username;
    private String firstName;
    private String lastName;
    private String pesel;

}
