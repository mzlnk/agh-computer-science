package pl.mzlnk.emergencyspotapi.utils.distance;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertTrue;

class DistanceUtilsTest {

    private static DistanceUtils distanceUtils;

    private static Coordinates coordsA = new Coordinates(18.991933, 50.097633);
    private static Coordinates coordsB = new Coordinates(19.031322, 50.262694);

    @BeforeAll
    static void setUp() {
        distanceUtils = new DistanceUtils();
    }

    @ParameterizedTest
    @MethodSource(value = "errorSupplier")
    void calculateDistanceTest(double error) {
        // assertTrue(Math.abs(distanceUtils.calculateDistance(coordsA, coordsB) - 24.36) <= error);
    }

    private static Stream<Double> errorSupplier() {
        return Stream
                .iterate(0D, i -> i + 1)
                .limit(10);
    }

}