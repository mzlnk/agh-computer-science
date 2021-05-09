package pl.mzlnk.conferencegenerator.generator.sql.impl.mssql;

import lombok.Getter;

import java.util.Calendar;
import java.util.stream.Stream;

@Getter
public enum MsSqlDataTypeEnum {

    INT("INT", int.class),
    VARCHAR("VARCHAR", String.class),
    TIMESTAMP("TIMESTAMP", Calendar.class);

    private String sqlDataType;
    private Class<?> javaEquivalent;

    private MsSqlDataTypeEnum(String sqlDataType, Class<?> javaEquivalent) {
        this.sqlDataType = sqlDataType;
        this.javaEquivalent = javaEquivalent;
    }

    public static MsSqlDataTypeEnum fromClass(Class<?> clazz) {
        return Stream.of(values())
                .filter(e -> e.javaEquivalent.equals(clazz))
                .findAny()
                .orElse(VARCHAR);
    }

}
