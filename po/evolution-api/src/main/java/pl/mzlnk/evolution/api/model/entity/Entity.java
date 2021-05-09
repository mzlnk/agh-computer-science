package pl.mzlnk.evolution.api.model.entity;

import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.world.EntityObserver;
import pl.mzlnk.evolution.api.model.world.World;
import pl.mzlnk.evolution.api.model.location.Location;

import java.util.UUID;

public interface Entity {

    UUID getUuid();

    EntityType getEntityType();
    EntityGenotype getGenotype();

    World getWorld();
    Location getLocation();

    void addObserver(EntityObserver observer);
    void removeObserver(EntityObserver observer);

}
