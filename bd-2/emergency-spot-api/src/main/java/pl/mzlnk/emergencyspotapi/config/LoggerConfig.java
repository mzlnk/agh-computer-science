package pl.mzlnk.emergencyspotapi.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import pl.mzlnk.emergencyspotapi.EmergencySpotApiApplication;

/**
 * Configuration class providing instance of Logger
 */
@Configuration
public class LoggerConfig {

    /**
     * Obtain Logger instance
     * @return Logger instance
     */
    @Bean
    public Logger logger() {
        return LoggerFactory.getLogger(EmergencySpotApiApplication.class);
    }

}
