package pl.mzlnk.conferencegenerator.model.entity.order;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;

import java.util.Calendar;

@Getter
@TableOrder(2)
@Table(name = "orders")
public class Order extends BaseEntity {

    @Builder
    public Order(int orderId, int buyerId, boolean cancelled, PaymentType paymentType, int value, Calendar date) {
        super(EntityType.ORDER, orderId);

        this.orderId = orderId;
        this.buyerId = buyerId;
        this.cancelled = cancelled;
        this.paymentType = paymentType;
        this.value = value;
        this.date = date;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "order_id")
    private int orderId;

    @ForeignKey(entity = Buyer.class, column = "buyer_id")
    @Column(name = "buyer_id")
    private int buyerId;

    @Column(name = "cancelled")
    private boolean cancelled;

    @Column(name = "payment_type")
    private PaymentType paymentType;

    @Column(name = "value")
    private int value;

    @Column(name = "date")
    private Calendar date;

}
