package pl.mzlnk.evolution.api.model.entity.impl;

import lombok.NonNull;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.World;

import static pl.mzlnk.evolution.api.model.entity.EntityType.GRASS;

public class Grass extends BaseLivingEntity {

    public Grass(@NonNull World world, @NonNull Location position) {
        super(GRASS, world, position, EntityGenotype.emptyGenotype(), 0);
    }

    @Override
    public int getEnergy() {
        return 10;
    }

    @Override
    public LivingEntity createChild(LivingEntity otherParent) {
        return null;
    }

}
