package pl.mzlnk.conferencegenerator.model.entity.order;

import java.util.Arrays;
import java.util.stream.Stream;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

public enum PaymentType {

    CASH("cash"),
    CHEQUE("cheque"),
    CREDIT_CARD("card"),
    TRANSFER("transfer"),
    NONE("null");

    private String name;

    private PaymentType(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }

    public static PaymentType randomPaymentType() {
        return Arrays.asList(PaymentType.values()).get(r.nextInt(values().length));
    }

}
