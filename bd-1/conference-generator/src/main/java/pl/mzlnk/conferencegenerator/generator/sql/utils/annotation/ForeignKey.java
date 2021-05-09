package pl.mzlnk.conferencegenerator.generator.sql.utils.annotation;

import pl.mzlnk.conferencegenerator.model.entity.Entity;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface ForeignKey {

    Class<? extends Entity> entity();
    String column();

}
