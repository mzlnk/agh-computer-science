package pl.mzlnk.evolution.api.model.evolution;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;
import pl.mzlnk.evolution.api.model.entity.MovableEntity;
import pl.mzlnk.evolution.api.model.entity.impl.Animal;
import pl.mzlnk.evolution.api.model.entity.impl.Grass;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.World;
import pl.mzlnk.evolution.api.model.world.WorldGenerator;
import pl.mzlnk.evolution.api.model.world.enums.WorldSector;

import java.util.*;
import java.util.stream.Collectors;

import static pl.mzlnk.evolution.api.utils.RandomUtil.r;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Evolution {

    public static Evolution newInstance(@NonNull EvolutionProperties properties) {
        Evolution evolution = new Evolution();

        evolution.uuid = properties.getId();
        evolution.world = WorldGenerator.fromProperties(properties);
        evolution.status = EvolutionStatus.RUNNING;
        evolution.properties = properties;

        for (int i = 0; i < evolution.properties.getAnimalInitAmount(); i++) {
            int x = r.nextInt(evolution.world.getWorldProperties().mapSize);
            int y = r.nextInt(evolution.world.getWorldProperties().mapSize);
            EntityGenotype genotype = EntityGenotype.randomGenotype();
            int generation = 1;

            evolution.world.place(new Animal(evolution.world, new Location(x, y), genotype, generation));
        }

        return evolution;
    }

    private UUID uuid;
    private World world;
    private EvolutionStatus status;
    private EvolutionProperties properties;

    private int currentDay = 0;

    public void setStatus(EvolutionStatus status) {
        this.status = status;
    }

    public void nextDay() {
        incrementDay();

        decreaseAnimalEnergy();
        rotateAnimals();
        moveAnimals();
        growAnimals();
        eatGrassByAnimals();
        breedAnimals();
        generateGrass();
        killAnimalsIfNecessary();

        changeEvolutionStatusIfNecessary();
    }

    private void rotateAnimals() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (MovableEntity) e)
                .forEach(MovableEntity::rotate);
    }

    private void growAnimals() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .forEach(Animal::grow);
    }

    private void moveAnimals() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (MovableEntity) e)
                .forEach(MovableEntity::move);
    }

    private void generateGrass() {
        List<Location> freeJungleLocations = world.findFreeLocations(WorldSector.JUNGLE);
        List<Location> freeWildernessLocations = world.findFreeLocations(WorldSector.WILDERNESS);

        if (freeJungleLocations.size() > 0) {
            Location loc = freeJungleLocations.get(r.nextInt(freeJungleLocations.size()));
            world.place(new Grass(world, loc));
        }

        if (freeWildernessLocations.size() > 0) {
            Location loc = freeWildernessLocations.get(r.nextInt(freeWildernessLocations.size()));
            world.place(new Grass(world, loc));
        }
    }

    private void eatGrassByAnimals() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .collect(Collectors.toMap(Entity::getLocation, e -> e, (e1, e2) -> e1.getEnergy() > e2.getEnergy() ? e1 : e2))
                .values()
                .forEach(animal -> {
                    world.findByLocation(animal.getLocation())
                            .stream()
                            .filter(e -> e.getEntityType().equals(EntityType.GRASS))
                            .findAny()
                            .ifPresent(grass -> {
                                animal.addEnergy(((Grass) grass).getEnergy());
                                world.remove(grass);
                            });
                });
    }

    private void decreaseAnimalEnergy() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (LivingEntity) e)
                .forEach(e -> e.reduceEnergy(1));
    }

    private void breedAnimals() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (Animal) e)
                .collect(Collectors.toMap(Entity::getLocation, Arrays::asList, (a1, a2) -> {
                    List<Animal> merged = new ArrayList<>();
                    merged.addAll(a1);
                    merged.addAll(a2);
                    return merged;
                }))
                .values()
                .forEach(list -> {
                    List<Animal> breedingAnimals = list
                            .stream()
                            .filter(e -> ((double) e.getEnergy() / properties.getMaxAnimalEnergy()) > 0.5)
                            .sorted(Comparator.comparingInt(Animal::getEnergy).reversed())
                            .collect(Collectors.toList());
                    if (breedingAnimals.size() >= 2) {
                        Animal child = (Animal) breedingAnimals.get(0).createChild(breedingAnimals.get(1));
                        world.place(child);
                    }
                });
    }

    private void killAnimalsIfNecessary() {
        world.findByType(EntityType.ANIMAL)
                .stream()
                .map(e -> (LivingEntity) e)
                .filter(e -> e.getEnergy() <= 0 || e.getEnergy() > properties.getLethalAnimalEnergy())
                .forEach(e -> world.remove(e));
    }

    private void incrementDay() {
        this.currentDay++;
        properties.setCurrentDay(this.currentDay);
    }

    private void changeEvolutionStatusIfNecessary() {
        if (this.currentDay >= properties.getLifetime()) {
            setStatus(EvolutionStatus.FINISHED);
        }
    }

}
