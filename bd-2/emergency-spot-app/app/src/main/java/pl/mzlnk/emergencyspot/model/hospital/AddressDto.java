package pl.mzlnk.emergencyspot.model.hospital;

import lombok.Data;

@Data
public class AddressDto {

    private String country;
    private String city;
    private String street;
    private Long streetNumber;

}
