package pl.mzlnk.evolution.api.model.world;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.evolution.EvolutionProperties;

import java.util.Map;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class WorldProperties {

    private static final int DEFAULT_INIT_ENERGY = 500;

    public static WorldProperties fromProperties(EvolutionProperties properties) {
        return new WorldProperties(properties);
    }

    private WorldProperties(EvolutionProperties properties) {
        this.mapSize = properties.getMapSize();
        this.jungleSize = properties.getJungleSize();

        this.initEntitiesEnergy = Map.of(
                EntityType.GRASS, properties.getGrassInitEnergy(),
                EntityType.ANIMAL, properties.getAnimalInitEnergy()
        );
    }

    public final int mapSize;
    public final int jungleSize;
    private final Map<EntityType, Integer> initEntitiesEnergy;

    public int getInitEnergy(EntityType entityType) {
        return initEntitiesEnergy.getOrDefault(entityType, DEFAULT_INIT_ENERGY);
    }

}