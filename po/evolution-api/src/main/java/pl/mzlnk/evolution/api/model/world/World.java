package pl.mzlnk.evolution.api.model.world;

import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.world.enums.WorldSector;
import pl.mzlnk.evolution.api.model.world.move_strategy.WorldMoveStrategy;
import pl.mzlnk.evolution.api.model.location.Location;

import java.util.List;

public interface World {

    WorldProperties getWorldProperties();
    WorldMoveStrategy getWorldMoveStrategy();

    List<Entity> findAllEntities();
    List<Entity> findByType(EntityType type);
    List<Entity> findByLocation(Location location);
    List<Entity> findBySector(WorldSector worldSector);

    List<Location> findOccupiedLocations(WorldSector... sectors);
    List<Location> findFreeLocations(WorldSector... sectors);

    void place(Entity entity);
    void remove(Entity entity);

    boolean canMoveTo(Location position);
    boolean isOccupied(Location position);

}
