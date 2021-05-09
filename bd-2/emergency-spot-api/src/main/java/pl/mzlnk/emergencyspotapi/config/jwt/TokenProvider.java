package pl.mzlnk.emergencyspotapi.config.jwt;

import io.jsonwebtoken.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Component class providing methods accessing given JWT token
 */
@Component
public class TokenProvider implements Serializable {

    @Value("${emergencyspotapi.jwt.access.token.validity}")
    private Long accessTokenValidity;

    @Value("${emergencyspotapi.jwt.secret}")
    private String secret;

    @Value("${emergencyspotapi.jwt.authorities.key}")
    private String authoritiesKey;

    /**
     * Obtain username from given token
     * @param token given JWT token
     * @return username associated with the token
     */
    public String getUsernameFromToken(String token) {
        return getClaimFromToken(token, Claims::getSubject);
    }

    /**
     * Obtain expiration date from given token
     * @param token given JWT token
     * @return expiration date of given token
     */
    public Date getExpirationDateFromToken(String token) {
        return getClaimFromToken(token, Claims::getExpiration);
    }

    public <T> T getClaimFromToken(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = getAllClaimsFromToken(token);
        return claimsResolver.apply(claims);
    }

    private Claims getAllClaimsFromToken(String token) {
        return Jwts.parser()
                .setSigningKey(this.secret)
                .parseClaimsJws(token)
                .getBody();
    }

    /**
     * Check if given token expired
     * @param token given JWT token
     * @return result whether token is expired or not
     */
    private Boolean isTokenExpired(String token) {
        final Date expiration = getExpirationDateFromToken(token);
        return expiration.before(new Date());
    }

    /**
     * Generate token based on provided authentication configuration
     * @param authentication provided request authentication
     * @return generated token
     */
    public String generateToken(Authentication authentication) {
        final String authorities = authentication.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.joining(","));

        return Jwts.builder()
                .setSubject(authentication.getName())
                .claim(this.authoritiesKey, authorities)
                .signWith(SignatureAlgorithm.HS256, this.secret)
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + this.accessTokenValidity * 1000))
                .compact();
    }

    /**
     * Validate token with provided user details
     * @param token given JWT token
     * @param userDetails given user detals
     * @return result whether token is validated
     */
    public Boolean validateToken(String token, UserDetails userDetails) {
        final String username = getUsernameFromToken(token);

        return (username.equals(userDetails.getUsername()) && !isTokenExpired(token));
    }

    /**
     * Obtain authentication from given JWT token and user details
     * @param token given JWT token
     * @param existingAuth existing authentication
     * @param userDetails given user details
     * @return authentication token
     */
    UsernamePasswordAuthenticationToken getAuthentication(final String token, final Authentication existingAuth, final UserDetails userDetails) {
        final JwtParser jwtParser = Jwts.parser().setSigningKey(this.secret);
        final Jws<Claims> claimsJws = jwtParser.parseClaimsJws(token);
        final Claims claims = claimsJws.getBody();
        final Collection<? extends GrantedAuthority> authorities = Arrays.stream(claims.get(this.authoritiesKey).toString().split(","))
                .map(SimpleGrantedAuthority::new)
                .collect(Collectors.toList());

        return new UsernamePasswordAuthenticationToken(userDetails, "", authorities);
    }

}
