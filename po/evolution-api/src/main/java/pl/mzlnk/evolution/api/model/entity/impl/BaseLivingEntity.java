package pl.mzlnk.evolution.api.model.entity.impl;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.World;

@Getter
public abstract class BaseLivingEntity extends BaseEntity implements LivingEntity {

    @Setter
    private int energy;

    private int generation;
    private int age = 0;

    public BaseLivingEntity(@NonNull EntityType entityType,
                            @NonNull World world,
                            @NonNull Location location,
                            @NonNull EntityGenotype genotype,
                            int generation) {
        super(entityType, world, location, genotype);
        this.generation = generation;
        this.energy = world.getWorldProperties().getInitEnergy(this.getEntityType());
    }

    @Override
    public void grow() {
        this.age++;
    }

    @Override
    public boolean hasEnergy() {
        return this.getEnergy() > 0;
    }

    @Override
    public void addEnergy(int energy) {
        this.energy += energy;
    }

    @Override
    public void reduceEnergy(int energy) {
        this.energy = Math.max(this.energy - energy, 0);
    }

}
