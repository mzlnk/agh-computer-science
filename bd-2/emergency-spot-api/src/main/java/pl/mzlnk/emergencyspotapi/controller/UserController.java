package pl.mzlnk.emergencyspotapi.controller;

import lombok.AllArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.emergencyspotapi.model.dto.user.NewUserDto;
import pl.mzlnk.emergencyspotapi.model.dto.user.UserDto;
import pl.mzlnk.emergencyspotapi.service.UserService;

import java.security.Principal;
import java.util.List;
import java.util.Optional;

/**
 * Controller dedicated to handle /users/* endpoints
 */
@CrossOrigin(origins = "", maxAge = 3600)
@RestController
@RequestMapping("/users")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    /**
     * Handle GET request to obtain list of all users
     * @return list of all users
     */
    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping
    public List<UserDto> findAll() {
        return userService.findAll();
    }

    /**
     * Handle GET request to obtain details of user with given ID
     * @param id user's unique ID
     * @return list of user details
     */
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/{id}")
    public Optional<UserDto> findOne(@PathVariable(value = "id") Long id) {
        return userService.findOne(id);
    }

    /**
     * Handle POST request to create new user with provided details
     * @param user user details
     */
    @PostMapping("/signup")
    public void saveUser(@RequestBody NewUserDto user) {
        this.userService.create(user);
    }

    /**
     * Handle GET request to obtain user username based on provided authentication information
     * @param principal instance containing user authentication data (gained from JWT token)
     * @return user details
     */
    @GetMapping("/me")
    public Optional<UserDto> getUsername(Principal principal) {
        return Optional.ofNullable(principal)
                .map(Principal::getName)
                .flatMap(userService::findByUsername);
    }


}
