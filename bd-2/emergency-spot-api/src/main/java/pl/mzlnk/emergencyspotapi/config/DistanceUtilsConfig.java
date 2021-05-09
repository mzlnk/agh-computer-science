package pl.mzlnk.emergencyspotapi.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import pl.mzlnk.emergencyspotapi.utils.distance.DistanceUtils;

/**
 * Configuration class providing instance of DistanceUtils
 */
@Configuration
public class DistanceUtilsConfig {

    /**
     * Obtain DistanceUtils instance
     * @return DistanceUtils instance
     */
    @Bean
    public DistanceUtils distanceUtils() {
        return new DistanceUtils();
    }

}
