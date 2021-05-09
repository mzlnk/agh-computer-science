package pl.mzlnk.emergencyspotapi.model.params;

import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Example;
import pl.mzlnk.emergencyspotapi.model.entity.HospitalWard;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class HospitalWardParamsTest {

    @Test
    void builder() {
        HospitalWardParams params = HospitalWardParams
                .builder()
                .build();

        assertNull(params.hospitalId);
        assertNull(params.wardType);
        assertEquals(0, params.minCapacity);
        assertEquals(Integer.MAX_VALUE, params.maxCapacity);
    }

    @Test
    void toExample() {
        Example<HospitalWard> example = HospitalWardParams
                .builder()
                .build()
                .toExample();

        assertNull(example.getProbe().getCapacity());
    }

}