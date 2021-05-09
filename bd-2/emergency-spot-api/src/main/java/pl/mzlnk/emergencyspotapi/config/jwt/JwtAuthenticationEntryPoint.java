package pl.mzlnk.emergencyspotapi.config.jwt;

import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.Serializable;

/**
 * Configuration class providing proper configuration for JWT token support
 */
@Component
public class JwtAuthenticationEntryPoint implements AuthenticationEntryPoint, Serializable {

    /**
     * <p>Commences an authentication scheme.</p>
     * <p>ExceptionTranslationFilter will populate the HttpSession attribute named AbstractAuthenticationProcessingFilter.SPRING_SECURITY_SAVED_REQUEST_KEY with the requested target URL before calling this method.</p>
     * <p>Implementations should modify the headers on the ServletResponse as necessary to commence the authentication process.</p>
     * @param request that resulted in an AuthenticationException
     * @param response so that the user agent can begin authentication
     * @param authException that caused the invocation
     * @throws IOException .
     */
    @Override
    public void commence(HttpServletRequest request,
                         HttpServletResponse response,
                         AuthenticationException authException) throws IOException {

        response.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized");
    }

}
