package pl.mzlnk.conferencegenerator.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public class CalendarUtil {

    private static final SimpleDateFormat FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    public static Calendar fromString(String str) {
        return fromString(str, "2000-01-01 12:00:00");
    }

    public static Calendar fromString(String str, String defaultValue) {
        Calendar cal = Calendar.getInstance();

        try {
            cal.setTime(FORMAT.parse(str));
        } catch (ParseException e) {
            try {
                cal.setTime(FORMAT.parse(defaultValue));
            } catch (ParseException e2) {
                e.printStackTrace();;
            }
        }

        return cal;
    }

    public static String toString(Calendar cal) {
        return FORMAT.format(cal.getTime());
    }

}
