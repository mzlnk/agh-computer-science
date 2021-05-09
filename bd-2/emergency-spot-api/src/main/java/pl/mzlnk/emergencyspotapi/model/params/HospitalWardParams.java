package pl.mzlnk.emergencyspotapi.model.params;

import lombok.Builder;
import org.springframework.data.domain.Example;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWardTypeEnum;

/**
 * Represents implementation of entity params for HospitalWard entity
 */
@Builder
public class HospitalWardParams implements EntityParams<HospitalWard> {

    public final HospitalWardTypeEnum wardType;

    @Builder.Default
    public final Integer minCapacity = 0;

    @Builder.Default
    public final Integer maxCapacity = Integer.MAX_VALUE;

    public final Long hospitalId;

    @Override
    public final Example<HospitalWard> toExample() {
        return Example.of(
                HospitalWard
                        .builder()
                        .wardType(this.wardType)
                        .hospital(Hospital
                                .builder()
                                .id(this.hospitalId)
                                .build()
                        )
                        .build()
        );
    }

}
