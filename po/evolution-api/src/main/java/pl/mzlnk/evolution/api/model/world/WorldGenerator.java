package pl.mzlnk.evolution.api.model.world;

import pl.mzlnk.evolution.api.model.evolution.EvolutionProperties;
import pl.mzlnk.evolution.api.model.world.impl.DesertWorld;

public class WorldGenerator {

    public static World fromProperties(EvolutionProperties properties) {
        return switch (properties.getWorldType()) {
            case DESERT -> new DesertWorld(WorldProperties.fromProperties(properties));
        };
    }

}
