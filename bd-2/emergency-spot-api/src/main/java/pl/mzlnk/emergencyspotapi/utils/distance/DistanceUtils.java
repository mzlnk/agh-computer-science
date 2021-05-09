package pl.mzlnk.emergencyspotapi.utils.distance;

import static java.lang.Math.*;

/**
 * Utility class to calculate distance between two locations with given coordinates
 */
public class DistanceUtils {

    /**
     * Earth's radius
     */
    private static final double R = 6371D;

    /**
     * Calculate distances in kilometers between two locations with given coordinates (longitude and latitude)
     * @param c1 first location
     * @param c2 second location
     * @return distance in kilometers
     */
    public double calculateDistance(Coordinates c1, Coordinates c2) {
        double dLatitude = Math.abs(c1.latitude() - c2.latitude()) * Math.PI / 180;
        double dLongitude = Math.abs(c1.longitude() - c2.longitude()) * Math.PI / 180;

        double a = pow(sin(dLatitude / 2), 2)
                + cos(c1.latitude() * Math.PI / 180)
                * cos(c2.latitude() * Math.PI / 180)
                * pow(sin(dLongitude / 2), 2);

        double c = 2 * atan2(sqrt(a), sqrt(1 - a));

        return R * c;
    }

}
