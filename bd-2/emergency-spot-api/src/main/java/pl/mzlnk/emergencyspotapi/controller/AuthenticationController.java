package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.config.jwt.TokenProvider;
import pl.mzlnk.emergencyspotapi.model.dto.user.UserDto;
import pl.mzlnk.emergencyspotapi.model.dto.user.AuthUserDto;
import pl.mzlnk.emergencyspotapi.service.UserService;

/**
 * Controller dedicated to handle /token/* endpoints
 */
//@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("/token")
@AllArgsConstructor
public class AuthenticationController {

    /**
     * Injected AuthenticationManager instance
     */
    private final AuthenticationManager authenticationManager;

    /**
     * Injected TokenProvider instance
     */
    private final TokenProvider tokenProvider;

    /**
     * Injected UserService instance
     */
    private final UserService userService;

    /**
     * Handle POST request to generate JWT token based on provided user details (username, password)
     * @param userDto DTO instance containing username and password
     * @return DTO instance containing authentication response (token, user ID, etc.)
     * @throws AuthenticationException if user credentails does not match to any user stored in database
     */
    @PostMapping(value = "/generate")
    public ResponseEntity<?> register(@RequestBody UserDto userDto) throws AuthenticationException {

        final Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(
                        userDto.getUsername(),
                        userDto.getPassword()
                )
        );
        SecurityContextHolder.getContext().setAuthentication(authentication);
        final String token = tokenProvider.generateToken(authentication);
        Long id = userService
                .findByUsername(userDto.getUsername())
                .map(UserDto::getId)
                .orElse(null);

        return ResponseEntity.ok(new AuthUserDto(id, userDto.getUsername(), token));
    }

}
