package pl.mzlnk.emergencyspotapi.model.dto.user;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * DTO class representing user's authentication details
 */
@Getter
@AllArgsConstructor
public class AuthUserDto {

    private Long patientId;
    private String username;
    private String token;

}
