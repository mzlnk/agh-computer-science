package pl.mzlnk.emergencyspotapi.utils.distance;

import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BiOptionalTest {

    @Test
    void ifPresentTest() {
        Optional<String> first = Optional.of("abc");
        Optional<Integer> second = Optional.of(1);

        BiOptional<String, Integer> biOptional = BiOptional.of(first, second);

        biOptional.ifPresentOrElse((f, s) -> {
                    assertEquals(f, "abc");
                    assertEquals(s, 1);
                },
                () -> {
                    throw new AssertionError();
                });
    }

}