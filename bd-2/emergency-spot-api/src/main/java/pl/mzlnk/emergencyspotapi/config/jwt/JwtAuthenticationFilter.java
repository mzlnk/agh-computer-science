package pl.mzlnk.emergencyspotapi.config.jwt;

import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.SignatureException;
import lombok.AllArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Filter base class that aims to guarantee a single execution per request dispatch, on any servlet container.
 */
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    @Autowired
    @Qualifier("userService")
    private UserDetailsService userDetailsService;

    @Autowired
    private TokenProvider jwtTokenUtil;

    @Value("${emergencyspotapi.jwt.access.token.validity}")
    private Long accessTokenValidity;

    @Value("${emergencyspotapi.jwt.secret}")
    private String secret;

    @Value("${emergencyspotapi.jwt.token.prefix}")
    private String tokenPrefix;

    @Value("${emergencyspotapi.jwt.header}")
    private String header;

    @Value("${emergencyspotapi.jwt.authorities.key}")
    private String authoritiesKey;

    /**
     * Same contract as for doFilter, but guaranteed to be just invoked once per request within a single request thread.
     * @throws IOException .
     * @throws ServletException .
     */
    @Override
    protected void doFilterInternal(HttpServletRequest req, HttpServletResponse res, FilterChain chain) throws IOException, ServletException {
        String header = req.getHeader(this.header);
        logger.info("Header: " + header);
        logger.info("Token prefix: " + this.tokenPrefix);
        String username = null;
        String authToken = null;
        if (header != null && header.startsWith(this.tokenPrefix)) {
            authToken = header.replace(this.tokenPrefix,"");
            try {
                username = jwtTokenUtil.getUsernameFromToken(authToken);
            } catch (IllegalArgumentException e) {
                logger.error("an error occured during getting username from token", e);
            } catch (ExpiredJwtException e) {
                logger.warn("the token is expired and not valid anymore", e);
            } catch(SignatureException e){
                logger.error("Authentication Failed. Username or Password not valid.");
            }
        } else {
            logger.warn("couldn't find bearer string, will ignore the header");
        }

        if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {

            UserDetails userDetails = userDetailsService.loadUserByUsername(username);

            if (jwtTokenUtil.validateToken(authToken, userDetails)) {
                UsernamePasswordAuthenticationToken authentication = jwtTokenUtil.getAuthentication(authToken, SecurityContextHolder.getContext().getAuthentication(), userDetails);
                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(req));
                logger.info("authenticated user " + username + ", setting security context");
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        }

        chain.doFilter(req, res);
    }

}
