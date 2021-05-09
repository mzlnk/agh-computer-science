package pl.mzlnk.evolution.api.model.world.impl;

import lombok.Getter;
import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.MovableEntity;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.EntityObserver;
import pl.mzlnk.evolution.api.model.world.World;
import pl.mzlnk.evolution.api.model.world.WorldProperties;
import pl.mzlnk.evolution.api.model.world.enums.WorldSector;
import pl.mzlnk.evolution.api.model.world.move_strategy.DefaultWorldMoveStrategy;
import pl.mzlnk.evolution.api.model.world.move_strategy.WorldMoveStrategy;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class BaseWorld implements World, EntityObserver {

    @Getter
    private WorldProperties worldProperties;

    @Getter
    protected WorldMoveStrategy worldMoveStrategy;

    private HashMap<Location, List<Entity>> entities = new HashMap<>();
    private HashMap<Location, WorldSector> sectors = new HashMap<>();

    public BaseWorld(WorldProperties properties) {
        this.worldProperties = properties;
        this.worldMoveStrategy = new DefaultWorldMoveStrategy();

        WorldSector.Util worldSectorUtil = new WorldSector.Util(properties);
        for (int x = 0; x < properties.mapSize; x++) {
            for (int y = 0; y < properties.mapSize; y++) {
                Location loc = new Location(x, y);
                entities.put(loc, new ArrayList<>());
                sectors.put(loc, worldSectorUtil.fromLocation(loc));
            }
        }
    }

    @Override
    public List<Entity> findAllEntities() {
        return entitiesStream()
                .collect(Collectors.toList());
    }

    @Override
    public List<Entity> findByType(EntityType type) {
        return entitiesStream()
                .filter(e -> e.getEntityType().equals(type))
                .collect(Collectors.toList());
    }

    @Override
    public List<Entity> findByLocation(Location location) {
        return entitiesStream()
                .filter(e -> e.getLocation().equals(location))
                .collect(Collectors.toList());
    }

    @Override
    public List<Entity> findBySector(WorldSector worldSector) {
        return sectors
                .entrySet()
                .stream()
                .filter(e -> e.getValue().equals(worldSector))
                .map(Map.Entry::getKey)
                .map(loc -> entities.get(loc))
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
    }

    @Override
    public List<Location> findOccupiedLocations(WorldSector... sectors) {
        List<WorldSector> sectorsList = Arrays.asList(sectors);
        return entities
                .entrySet()
                .stream()
                .filter(e -> sectorsList.contains(this.sectors.get(e.getKey())))
                .filter(e -> !e.getValue().isEmpty())
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    @Override
    public List<Location> findFreeLocations(WorldSector... sectors) {
        List<WorldSector> sectorsList = Arrays.asList(sectors);
        return entities
                .entrySet()
                .stream()
                .filter(e -> sectorsList.contains(this.sectors.get(e.getKey())))
                .filter(e -> e.getValue().isEmpty())
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    @Override
    public void place(Entity entity) {
        entities.get(entity.getLocation()).add(entity);
        entity.addObserver(this);
    }

    @Override
    public void remove(Entity entity) {
        entities.get(entity.getLocation()).remove(entity);
        entity.removeObserver(this);
    }

    @Override
    public boolean canMoveTo(Location position) {
        return this.findByLocation(position)
                .stream()
                .filter(e -> e instanceof MovableEntity)
                .count() < 3; // todo: inspect during tests
    }

    @Override
    public boolean isOccupied(Location position) {
        return entities.containsKey(position);
    }

    @Override
    public void onPositionChanged(Entity entity, Location oldPosition, Location newPosition) {
        entities.get(oldPosition).remove(entity);
        entities.get(newPosition).add(entity);
    }

    private Stream<Entity> entitiesStream() {
        return entities
                .values()
                .stream()
                .flatMap(Collection::stream);
    }

}
