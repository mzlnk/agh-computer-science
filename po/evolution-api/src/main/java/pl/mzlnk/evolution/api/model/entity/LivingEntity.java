package pl.mzlnk.evolution.api.model.entity;

public interface LivingEntity extends Entity {

    int getEnergy();
    boolean hasEnergy();

    int getGeneration();
    int getAge();

    void grow();

    void setEnergy(int energy);
    void addEnergy(int energy);
    void reduceEnergy(int energy);

    LivingEntity createChild(LivingEntity otherParent);

}
