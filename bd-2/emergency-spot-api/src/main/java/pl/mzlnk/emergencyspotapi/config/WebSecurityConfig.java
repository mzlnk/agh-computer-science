package pl.mzlnk.emergencyspotapi.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import pl.mzlnk.emergencyspotapi.config.jwt.JwtAuthenticationEntryPoint;
import pl.mzlnk.emergencyspotapi.config.jwt.JwtAuthenticationFilter;

import javax.annotation.Resource;

/**
 * Configuration class providing config essential to properly secure resource servers and its endpoints using JWT tokens
 */
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

    /**
     * Injected UserDetailsService instance
     */
    @Resource(name = "userService")
    private UserDetailsService userDetailsService;

    /**
     * Injected JwtAuthenticationEntryPoint configuration instance
     */
    @Autowired
    private JwtAuthenticationEntryPoint unauthorizedHandler;

    /**
     * Obtain AuthenticationManager instance
     * @return AuthenticationManager instance
     * @throws Exception
     */
    @Override
    @Bean
    public AuthenticationManager authenticationManagerBean() throws Exception {
        return super.authenticationManagerBean();
    }

    /**
     * Set configuration for user details
     * @throws Exception if setting details failed
     */
    @Autowired
    public void globalUserDetails(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(userDetailsService)
                .passwordEncoder(encoder());
    }

    /**
     * Obtain JwtAuthenticationFilter instance
     * @return JwtAuthenticationFilter instance
     */
    @Bean
    public JwtAuthenticationFilter authenticationTokenFilterBean() {
        return new JwtAuthenticationFilter();
    }

    /**
     * <p>Configure resource server endpoints' security.</p>
     * <p>Indicate which endpoints should be authenticated or permitted without authentication</p>
     * <p>Attach configuration classes for JWT token support (filters, entrypoints, etc.)</p>
     * @param http HttpSecurity instance
     * @throws Exception if configuration failed
     */
    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.cors().and().csrf().disable()
                .authorizeRequests()
                .antMatchers("/users/signup", "/token/generate", "/hospitals/**", "/wards/**", "/stays/**").permitAll()
                .anyRequest().authenticated()
                .and()
                .exceptionHandling().authenticationEntryPoint(unauthorizedHandler).and()
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        http
                .addFilterBefore(authenticationTokenFilterBean(), UsernamePasswordAuthenticationFilter.class);
    }

    /**
     * <p>Configure resource server endpoints' security.</p>
     * <p>Indicate which endpoints should be ignored in authentication with JWT tokens</p>
     * @param webSecurity WebSecurity instance
     */
    @Override
    public void configure(WebSecurity webSecurity) {
        webSecurity
                .ignoring()
                .antMatchers("/users/signup", "/token/generate", "/hospitals", "/wards");
    }

    /**
     * Obtain BCryptPasswordEncoder instance used to encrypt passwords stored in database
     * @return BCryptPasswordEncoder
     */
    @Bean
    public BCryptPasswordEncoder encoder(){
        return new BCryptPasswordEncoder();
    }

}
