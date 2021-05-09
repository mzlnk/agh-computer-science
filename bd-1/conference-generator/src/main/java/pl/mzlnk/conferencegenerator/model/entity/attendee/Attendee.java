package pl.mzlnk.conferencegenerator.model.entity.attendee;

import lombok.Builder;
import lombok.Getter;
import pl.mzlnk.conferencegenerator.model.entity.BaseEntity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;
import pl.mzlnk.conferencegenerator.generator.sql.utils.annotation.*;

@Getter
@TableOrder(1)
@Table(name = "attendee")
public class Attendee extends BaseEntity {

    @Builder
    public Attendee(int attendeeId, String firstName, String lastName, String email, int companyId) {
        super(EntityType.ATTENDEE, attendeeId);

        this.attendeeId = attendeeId;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;
        this.companyId = companyId;
    }

    @PrimaryKey
    @AutoIncrement
    @Column(name = "attendee_id")
    private int attendeeId;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "last_name")
    private String lastName;

    @Column(name = "email")
    private String email;

    @ForeignKey(entity = Company.class, column = "company_id")
    @Column(name = "company_id")
    private int companyId;

}
