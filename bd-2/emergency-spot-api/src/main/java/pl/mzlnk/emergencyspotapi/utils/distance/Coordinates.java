package pl.mzlnk.emergencyspotapi.utils.distance;

import lombok.AllArgsConstructor;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;

/**
 * Represents location coordinates (longitude and latitude)
 */
@AllArgsConstructor
public final class Coordinates {

    private final double longitude;
    private final double latitude;

    /**
     * Obtain coordinates from hospital entity instance
     * @param hospital hospital entity instance
     * @return Coordinates instance
     */
    public static Coordinates fromHospital(Hospital hospital) {
        return new Coordinates(hospital.getLongitude(), hospital.getLatitude());
    }

    /**
     * Return longitude
     * @return longitude
     */
    public double longitude() {
        return longitude;
    }

    /**
     * Return latitude
     * @return latitude
     */
    public double latitude() {
        return latitude;
    }

}
