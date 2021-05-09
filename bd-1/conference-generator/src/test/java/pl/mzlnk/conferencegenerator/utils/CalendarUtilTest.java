package pl.mzlnk.conferencegenerator.utils;

import org.junit.jupiter.api.Test;

import java.util.Calendar;

import static org.junit.jupiter.api.Assertions.*;

class CalendarUtilTest {

    @Test
    void currentTime() {
        System.out.println(Calendar.getInstance().getTime().getTime());
    }

}