package pl.mzlnk.evolution.api.model.world;

import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.location.Location;

public interface EntityObserver {

    void onPositionChanged(Entity entity, Location oldPosition, Location newPosition);

}
