package pl.mzlnk.emergencyspotapi.model.params;

import lombok.Builder;
import lombok.Singular;
import org.springframework.data.domain.Example;
import pl.mzlnk.emergencyspotapi.model.entity.Address;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

import java.util.List;


/**
 * Represents implementation of entity params for Hospital etity
 */
@Builder
public class HospitalParams implements EntityParams<Hospital> {

    public final String name;
    public final Double longitude;
    public final Double latitude;
    public final String country;
    public final String city;

    @Singular
    public final List<HospitalWardTypeEnum> wards;

    @Override
    public final Example<Hospital> toExample() {
        return Example.of(
                Hospital
                        .builder()
                        .name(this.name)
                        .longitude(this.longitude)
                        .latitude(this.latitude)
                        .address(Address.builder()
                                .country(this.country)
                                .city(this.city)
                                .build())
                        .build()
        );
    }

}
