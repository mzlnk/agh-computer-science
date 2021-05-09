package pl.mzlnk.evolution.api.model.evolution;

import lombok.AllArgsConstructor;
import lombok.Data;
import pl.mzlnk.evolution.api.model.world.enums.WorldType;

import java.util.UUID;

@Data
@AllArgsConstructor
public class EvolutionProperties {

    private UUID id;
    private String name;
    private WorldType worldType;
    private int mapSize;
    private int jungleSize;
    private long lifetime;
    private long currentDay;
    private int maxAnimalEnergy;
    private int lethalAnimalEnergy;
    private int animalInitEnergy;
    private int grassInitEnergy;
    private int animalInitAmount;

    public boolean isValid() {
        if (id == null) return false;
        if (name == null) return false;
        if (worldType == null) return false;
        if (mapSize <= jungleSize) return false;
        return mapSize * jungleSize * lifetime * maxAnimalEnergy * animalInitEnergy * grassInitEnergy > 0;
    }

}
