package pl.mzlnk.conferencegenerator.model;

import org.junit.jupiter.api.Test;
import pl.mzlnk.conferencegenerator.model.entity.order.PaymentType;

class PaymentTypeTest {

    @Test
    void toStringTest() {
        System.out.println(PaymentType.CREDIT_CARD);
    }

}