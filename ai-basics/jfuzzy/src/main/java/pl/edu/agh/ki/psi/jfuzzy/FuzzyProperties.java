package pl.edu.agh.ki.psi.jfuzzy;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.math.NumberUtils;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class FuzzyProperties {

    private static final double DEFAULT_ROOF_SLOPE = 25D;
    private static final double DEFAULT_ROOF_DIRECTION = 180D;
    private static final double DEFAULT_SHADINESS = 50D;
    private static final int DEFAULT_DAY_OF_YEAR = 180;

    private String fclFilename;
    private double roofSlope;
    private double roofDirection;
    private double shadiness;
    private int dayOfYear;

    public static FuzzyProperties fromArguments(String[] args) {
        FuzzyProperties properties = new FuzzyProperties();

        if (args.length != 5) {
            throw new IllegalStateException("Invalid argument size. Should be 5 instead of " + args.length);
        }

        properties.fclFilename = args[0];
        properties.roofSlope = NumberUtils.toDouble(args[1], DEFAULT_ROOF_SLOPE);
        properties.roofDirection = NumberUtils.toDouble(args[2], DEFAULT_ROOF_DIRECTION);
        properties.shadiness = NumberUtils.toDouble(args[3], DEFAULT_DAY_OF_YEAR);
        properties.dayOfYear = NumberUtils.toInt(args[4], DEFAULT_DAY_OF_YEAR);

        return properties;
    }

    @Override
    public String toString() {
        return String.format("Fuzzy properties:\nfclFilename: %s\nroof slope: %.2f\nroof direction: %.2f\nshadiness: %.2f%%\nday of year: %d",
                fclFilename,
                roofSlope,
                roofDirection,
                shadiness,
                dayOfYear
        );
    }
}
