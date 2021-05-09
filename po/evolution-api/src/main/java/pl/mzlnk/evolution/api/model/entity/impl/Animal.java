package pl.mzlnk.evolution.api.model.entity.impl;

import lombok.Getter;
import lombok.NonNull;
import pl.mzlnk.evolution.api.model.entity.EntityType;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;
import pl.mzlnk.evolution.api.model.entity.MovableEntity;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotype;
import pl.mzlnk.evolution.api.model.genotype.EntityGenotypeCreator;
import pl.mzlnk.evolution.api.model.genotype.ParentBasedGenotypeCreator;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.World;
import pl.mzlnk.evolution.api.model.world.enums.WorldDirectionEnum;

import static pl.mzlnk.evolution.api.model.world.enums.WorldDirectionEnum.UNIT_DEGREE_CHANGE;
import static pl.mzlnk.evolution.api.utils.RandomUtil.r;

@Getter
public class Animal extends BaseLivingEntity implements MovableEntity {

    private WorldDirectionEnum mapDirection = WorldDirectionEnum.randomMapDirection();


    public Animal(@NonNull World world,
                  @NonNull Location location,
                  @NonNull EntityGenotype genotype,
                  int generation) {
        super(EntityType.ANIMAL, world, location, genotype, generation);
    }


    @Override
    public void move() {
        Location unitVector = mapDirection.getUnitVector();
        Location newPosition = this.getLocation().add(unitVector, this.getWorld().getWorldMoveStrategy());
        if (this.getWorld().canMoveTo(newPosition)) {
            this.setLocation(newPosition);
        }
    }

    @Override
    public void rotate() {
        mapDirection = mapDirection.rotate(UNIT_DEGREE_CHANGE * this.getGenotype().geneAt(r.nextInt(32)));
    }

    @Override
    public LivingEntity createChild(LivingEntity otherParent) {
        EntityGenotypeCreator creator = new ParentBasedGenotypeCreator(this.getGenotype(), otherParent.getGenotype());

        LivingEntity child = new Animal(
                this.getWorld(),
                this.getLocation(),
                creator.createGenotype(),
                Math.max(this.getGeneration(), otherParent.getGeneration()) + 1);

        child.setEnergy((int) (0.5 * (this.getEnergy() + otherParent.getEnergy())));
        this.setEnergy((int) (0.75 * this.getEnergy()));
        otherParent.setEnergy((int) (0.75 * this.getEnergy()));

        return child;
    }

}
