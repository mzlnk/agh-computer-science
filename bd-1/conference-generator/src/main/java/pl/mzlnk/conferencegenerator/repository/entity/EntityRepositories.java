package pl.mzlnk.conferencegenerator.repository.entity;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.model.entity.EntityType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EntityRepositories {

    public static EntityRepositories init() {
        EntityRepositories repositories = new EntityRepositories();

        repositories.registerRepository(new AttendeeRepository());
        repositories.registerRepository(new BuyerRepository());
        repositories.registerRepository(new CompanyRepository());
        repositories.registerRepository(new ConferenceDayAttendeeRepository());
        repositories.registerRepository(new ConferenceDayOrderItemRepository());
        repositories.registerRepository(new ConferenceDayPriceRepository());
        repositories.registerRepository(new ConferenceDayRepository());
        repositories.registerRepository(new ConferenceRepository());
        repositories.registerRepository(new OrderRepository());
        repositories.registerRepository(new StudentAttendeeRepository());
        repositories.registerRepository(new WorkshopAttendeeRepository());
        repositories.registerRepository(new WorkshopOrderItemRepository());
        repositories.registerRepository(new WorkshopRepository());

        return repositories;
    }

    private Map<EntityType, EntityRepository<? extends Entity>> repositories = new HashMap<>();

    public List<EntityRepository<? extends Entity>> getAllRepositories() {
        return new ArrayList<>(repositories.values());
    }

    public <E extends Entity> EntityRepository<E> getRepository(EntityType entityType) {
        return (EntityRepository<E>) repositories.get(entityType);
    }

    private void registerRepository(EntityRepository<? extends Entity> repository) {
        repositories.put(repository.getEntityType(), repository);
    }

}
