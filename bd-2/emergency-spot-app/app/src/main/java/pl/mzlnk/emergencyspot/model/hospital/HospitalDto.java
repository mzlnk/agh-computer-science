package pl.mzlnk.emergencyspot.model.hospital;

import lombok.Data;

@Data
public class HospitalDto {

    private Long id;
    private String name;
    private String description;
    private AddressDto address;

}
