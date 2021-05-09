package pl.mzlnk.conferencegenerator.generator.sql.impl.mssql;

import org.junit.jupiter.api.Test;
import pl.mzlnk.conferencegenerator.model.entity.attendee.Attendee;

import java.lang.reflect.Field;

class MsSqlDataTypeEnumTest {

    @Test
    void fromClass() {
        for (Field field : Attendee.class.getDeclaredFields()) {
            System.out.println("field: " + field.getType().getName());
        }
    }
}