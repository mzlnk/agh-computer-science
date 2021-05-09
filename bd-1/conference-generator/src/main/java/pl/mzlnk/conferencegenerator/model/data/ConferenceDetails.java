package pl.mzlnk.conferencegenerator.model.data;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;

@Data
@AllArgsConstructor
public class ConferenceDetails {

    private String name;
    private String description;
    private double studentDiscount;

}
