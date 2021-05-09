package pl.mzlnk.evolution.api.model.evolution;

import lombok.AccessLevel;
import lombok.Getter;
import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.impl.Animal;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Getter
public class EvolutionStatistics {

    public static EvolutionStatistics fromEvolution(Evolution evolution) {
        return new EvolutionStatistics(evolution);
    }

    @Getter(AccessLevel.NONE)
    private Evolution evolution;

    private long animals;
    private long grass;
    private long all;

    private double animalAverageEnergy;

    private int animalMinAge;
    private int animalAverageAge;
    private int animalMaxAge;

    private long animalMinGeneration;
    private long animalMaxGeneration;

    private EntityGenotype dominantGenotype;

    private Map<Double, Integer> animalsEnergyDistribution = new HashMap<>();
    private Map<Integer, Integer> animalsGenerationDistribution = new HashMap<>();
    private Map<Integer, Integer> animalsGenesDistribution = new HashMap<>();

    private EvolutionStatistics(Evolution evolution) {
        this.evolution = evolution;
        processOccurrences();
        processAgeAndGeneration();
        processEnergy();
        processGenotype();
    }

    private void processOccurrences() {
        this.all = evolution.getWorld()
                .findAllEntities()
                .stream()
                .peek(e -> {
                    switch (e.getEntityType()) {
                        case GRASS -> this.grass++;
                        case ANIMAL -> this.animals++;
                    }
                })
                .count();
    }

    private void processAgeAndGeneration() {
        this.animalAverageAge = (int) this.evolution.getWorld()
                .findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .peek(e -> {
                    this.animalMinAge = Math.min(this.animalMinAge, e.getAge());
                    this.animalMaxAge = Math.max(this.animalMaxAge, e.getAge());
                    this.animalMinGeneration = Math.min(this.animalMinGeneration, e.getGeneration());
                    this.animalMaxGeneration = Math.max(this.animalMaxGeneration, e.getGeneration());

                    this.animalsGenerationDistribution.putIfAbsent(e.getGeneration(), 0);
                    this.animalsGenerationDistribution.compute(e.getGeneration(), (k, v) -> v += 1);
                })
                .mapToInt(Animal::getAge)
                .average()
                .orElse(0);
    }

    private void processEnergy() {
        this.animalAverageEnergy = this.evolution.getWorld()
                .findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .peek(e -> {
                    double ratio = Math.round(100D * e.getEnergy() / evolution.getProperties().getMaxAnimalEnergy()) / 100D;
                    this.animalsEnergyDistribution.putIfAbsent(ratio, 0);
                    this.animalsEnergyDistribution.compute(ratio, (k, v) -> v += 1);
                })
                .mapToDouble(Animal::getEnergy)
                .average()
                .orElse(0);
    }

    private void processGenotype() {
        List<EntityGenotype> genotypes = this.evolution.getWorld()
                .findByType(EntityType.ANIMAL)
                .stream()
                .peek(e -> {
                    e.getGenotype().genesStream()
                            .forEach(g -> {
                                animalsGenesDistribution.putIfAbsent(g, 0);
                                animalsGenesDistribution.compute(g, (k, v) -> v += 1);
                            });
                })
                .collect(Collectors.toMap(Entity::getGenotype, e -> 1, Integer::sum))
                .entrySet()
                .stream()
                .sorted(Comparator.comparingInt(Map.Entry::getValue))
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());

        this.dominantGenotype = genotypes.isEmpty() ? null : genotypes.get(0);
    }

}
