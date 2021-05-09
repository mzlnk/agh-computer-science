package pl.mzlnk.conferencegenerator.generator.sql.utils.parser;

import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;
import pl.mzlnk.conferencegenerator.utils.CalendarUtil;

import java.lang.reflect.Field;
import java.util.Calendar;
import java.util.GregorianCalendar;

public class SqlEntityFieldParser {

    private final Entity entity;
    private final Field field;

    public SqlEntityFieldParser(Field field) {
        this(field, null);
    }

    public SqlEntityFieldParser(Field field, Entity entity) {
        this.field = field;
        this.entity = entity;

        this.field.setAccessible(true);
    }

    public String getStringValue() {
        try {
            var val = field.get(entity);
            if(val.getClass().equals(Integer.class)) {
                int i = (Integer) val;
                return i != -1 ? String.valueOf(val) : "null";
            }
            if(val.getClass().equals(Double.class)) {
                double d = (Double) val;
                return d != -1 ? String.valueOf(val) : "null";
            }
            if(val.getClass().equals(Boolean.class)) {
                boolean b = (Boolean) val;
                return b ? "1" : "0";
            }
            if(val.getClass().equals(GregorianCalendar.class)) return "'" + CalendarUtil.toString((Calendar) val) + "'";
            if(String.valueOf(field.get(entity)).equals("null")) return "null";
            return "'" + field.get(entity) + "'";
        } catch (IllegalAccessException e) {
            return ""; // todo: add log support here
        }
    }

    public String getColumnName() {
        return field.getDeclaredAnnotation(Column.class).name();
    }

    public Class<?> getColumnType() {
        return field.getType();
    }

    public boolean isColumn() {
        return field.getDeclaredAnnotation(Column.class) != null;
    }

    public boolean isPrimaryKey() {
        return field.getDeclaredAnnotation(PrimaryKey.class) != null;
    }

    public boolean isForeignKey() {
        return field.getDeclaredAnnotation(ForeignKey.class) != null;
    }

    public boolean isAutoIncrement() {
        return field.getDeclaredAnnotation(AutoIncrement.class) != null;
    }

    public boolean isNotNull() {
        return field.getDeclaredAnnotation(NotNull.class) != null;
    }

    public String getForeignTableName() {
        return field.getDeclaredAnnotation(ForeignKey.class).entity().getDeclaredAnnotation(Table.class).name();
    }

    public String getForeignColumnName() {
        return field.getDeclaredAnnotation(ForeignKey.class).column();
    }

}
