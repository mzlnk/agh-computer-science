package pl.mzlnk.evolution.api.model.evolution;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;
import pl.mzlnk.evolution.api.model.entity.impl.Animal;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotypeUtil;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Getter
public class EvolutionEntitiesOverview {

    private static final int MAX_ENTITIES_DETAILS = 100;

    public static EvolutionEntitiesOverview fromEvolution(Evolution evolution) {
        int maxEnergy = evolution.getProperties().getMaxAnimalEnergy();
        int lethalEnergy = evolution.getProperties().getLethalAnimalEnergy();

        EvolutionEntitiesOverview entitiesOverview = new EvolutionEntitiesOverview(maxEnergy, lethalEnergy);

        evolution.getWorld().findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .sorted(Comparator.comparingInt(LivingEntity::getAge).reversed())
                .forEach(entitiesOverview::addEntity);

        return entitiesOverview;
    }

    private List<Integer> age = new ArrayList<>();
    private List<Integer> generation = new ArrayList<>();
    private List<Integer[]> genes = new ArrayList<>();
    private List<Double> energy = new ArrayList<>();
    private List<Double> realEnergy = new ArrayList<>();

    private int count;
    private boolean more;

    @Getter(AccessLevel.NONE)
    private int maxEnergy;
    @Getter(AccessLevel.NONE)
    private int lethalEnergy;

    private EvolutionEntitiesOverview(int maxEnergy, int lethalEnergy) {
        this.maxEnergy = maxEnergy;
        this.lethalEnergy = lethalEnergy;
    }

    private void addEntity(LivingEntity entity) {
        this.count++;
        if (count > MAX_ENTITIES_DETAILS) {
            this.more = true;
            return;
        }
        int age = entity.getAge();
        int generation = entity.getGeneration();
        double energy = Math.min(Math.round(100D * entity.getEnergy() / maxEnergy) / 100D, 1.0D);
        double realEnergy = 100D * entity.getEnergy() / lethalEnergy;

        EntityGenotypeUtil geneUtil = new EntityGenotypeUtil(entity.getGenotype());
        Integer[] genes = geneUtil.getGenesCountList();

        this.age.add(age);
        this.generation.add(generation);
        this.genes.add(genes);
        this.energy.add(energy);
        this.realEnergy.add(realEnergy);

    }

}
