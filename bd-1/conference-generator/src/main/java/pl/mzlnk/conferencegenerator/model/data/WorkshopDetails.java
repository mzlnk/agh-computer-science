package pl.mzlnk.conferencegenerator.model.data;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;

import java.util.Calendar;

@Data
@AllArgsConstructor
public class WorkshopDetails {

    private String name;
    private String description;
    private int price;
    private int attendeesLimit;

}
