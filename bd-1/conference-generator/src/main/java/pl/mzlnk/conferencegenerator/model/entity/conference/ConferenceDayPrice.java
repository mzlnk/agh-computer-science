package pl.mzlnk.conferencegenerator.model.entity.conference;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;

import java.util.Calendar;

@Getter
@TableOrder(2)
@Table(name = "conference_day_price")
public class ConferenceDayPrice extends BaseEntity {

    @Builder
    public ConferenceDayPrice(int priceId, int conferenceDayId, int value, String description, Calendar expirationDate) {
        super(EntityType.CONFERENCE_DAY_PRICE, priceId);

        this.priceId = priceId;
        this.conferenceDayId = conferenceDayId;
        this.value = value;
        this.description = description;
        this.expirationDate = expirationDate;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "price_id")
    private int priceId;

    @ForeignKey(entity = ConferenceDay.class, column = "conference_day_id")
    @Column(name = "conference_day_id")
    private int conferenceDayId;

    @Column(name = "value")
    private int value;

    @Column(name = "description")
    private String description;

    @Column(name = "expiration_date")
    private Calendar expirationDate;

}
