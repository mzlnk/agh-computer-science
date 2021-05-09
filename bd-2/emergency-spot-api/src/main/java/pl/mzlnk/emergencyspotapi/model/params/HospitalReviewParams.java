package pl.mzlnk.emergencyspotapi.model.params;

import lombok.Builder;
import org.springframework.data.domain.Example;
import pl.mzlnk.emergencyspotapi.model.entity.Hospital;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalReview;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

/**
 * Represents implementation of entity params for HospitalReview entity
 */
@Builder
public class HospitalReviewParams implements EntityParams<HospitalReview> {

    @Builder.Default
    public final Double minRating = 0D;

    @Builder.Default
    public final Double maxRating = 10D;

    public final Long hospitalId;

    @Override
    public final Example<HospitalReview> toExample() {
        return Example.of(
                HospitalReview
                        .builder()
                        .hospitalWard(
                                HospitalWard
                                        .builder()
                                        .hospital(
                                                Hospital
                                                        .builder()
                                                        .id(this.hospitalId)
                                                        .build()
                                        )
                                .build()
                        )
                        .build()
        );
    }

}
