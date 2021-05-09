package pl.mzlnk.evolution.api.model.entity.impl;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.EntityObserver;
import pl.mzlnk.evolution.api.model.world.World;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Getter
@ToString
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public abstract class BaseEntity implements Entity {

    private static final Location DEFAULT_LOCATION = new Location(0, 0);

    @EqualsAndHashCode.Include
    private UUID uuid;
    private EntityType entityType;

    private EntityGenotype genotype;

    private World world;
    private Location location;

    private List<EntityObserver> observers = new ArrayList<>();

    public BaseEntity(@NonNull EntityType entityType,
                      @NonNull World world,
                      @NonNull Location location,
                      @NonNull EntityGenotype genotype) {
        this.uuid = UUID.randomUUID();
        this.entityType = entityType;
        this.world = world;
        this.location = location;
        this.genotype = genotype;
    }

    @Override
    public void addObserver(EntityObserver observer) {
        this.observers.add(observer);
    }

    @Override
    public void removeObserver(EntityObserver observer) {
        this.observers.remove(observer);
    }

    protected final void setLocation(Location position) {
        Location oldPosition = this.getLocation();
        this.location = position;

        for (EntityObserver observer : observers) {
            observer.onPositionChanged(this, oldPosition, this.location);
        }
    }

}
