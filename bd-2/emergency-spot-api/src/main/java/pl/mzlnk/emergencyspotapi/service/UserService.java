package pl.mzlnk.emergencyspotapi.service;

import pl.mzlnk.emergencyspotapi.model.dto.user.NewUserDto;
import pl.mzlnk.emergencyspotapi.model.dto.user.UserDto;
import pl.mzlnk.emergencyspotapi.model.entity.User;

import java.util.List;
import java.util.Optional;

/**
 * Represents an API for user service which mediates between controller and repository layers
 */
public interface UserService {

    /**
     * Obtain list of all users
     * @return list of all users
     */
    List<UserDto> findAll();

    /**
     * Obtain details about user with given ID
     * @param id users's unique ID
     * @return users details or null if user with given ID does not exist
     */
    Optional<UserDto> findOne(Long id);

    /**
     * Obtain details about user with given username
     * @param username user username
     * @return users details or null if user with given username does not exist
     */
    Optional<UserDto> findByUsername(String username);

    /**
     * Delete existing user
     * @param id user's unique ID
     */
    void deleteById(Long id);

    /**
     * Create new user
     * @param userDto DTO instance representing information about creating user
     * @return details of created user
     */
    User create(NewUserDto userDto);

}
