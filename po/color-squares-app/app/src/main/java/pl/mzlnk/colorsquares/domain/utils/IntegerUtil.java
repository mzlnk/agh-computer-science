package pl.mzlnk.colorsquares.domain.utils;

public class IntegerUtil {

    public static boolean between(int v1, int v2, int v3, boolean includeLimitValues) {
        return (includeLimitValues ? (v1 >= v2 && v1 <= v3) : (v1 > v2 && v1 < v3));
    }

}
