package pl.mzlnk.conferencegenerator.generator.entity;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.conferencegenerator.generator.entity.utils.comparator.EntityGeneratorDependencyComparator;
import pl.mzlnk.conferencegenerator.repository.data.DataRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EntityGenerators {

    public static EntityGenerators init(EntityRepositories entityRepositories, DataRepositories dataRepositories) {
        EntityGenerators generators = new EntityGenerators();

        EntityGenerator e1 = new AttendeeGenerator(entityRepositories, dataRepositories);
        EntityGenerator e2 = new BuyerGenerator(entityRepositories, dataRepositories);

        generators.registerGenerator(new AttendeeGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new BuyerGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new CompanyGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new ConferenceGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new ConferenceDayAttendeeGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new ConferenceDayGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new ConferenceDayOrderItemGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new ConferenceDayPriceGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new OrderGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new StudentAttendeeGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new WorkshopAttendeeGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new WorkshopOrderItemGenerator(entityRepositories, dataRepositories));
        generators.registerGenerator(new WorkshopGenerator(entityRepositories, dataRepositories));

        generators.registerCleanupGenerator(new CleanupGenerator(entityRepositories, dataRepositories));

        return generators;
    }

    private List<EntityGenerator> generators = new ArrayList<>();
    private List<EntityGenerator> cleanupGenerators = new ArrayList<>();

    public void generate() {
        this.generators
                .stream()
                .sorted(new EntityGeneratorDependencyComparator())
                .forEach(EntityGenerator::generate);

        // this.cleanupGenerators.forEach(EntityGenerator::generate);
    }

    private void registerGenerator(EntityGenerator generator) {
        generators.add(generator);
    }

    private void registerCleanupGenerator(EntityGenerator generator) {
        cleanupGenerators.add(generator);
    }

}
