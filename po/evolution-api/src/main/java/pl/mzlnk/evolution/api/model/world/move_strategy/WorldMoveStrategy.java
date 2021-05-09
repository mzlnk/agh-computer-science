package pl.mzlnk.evolution.api.model.world.move_strategy;

import pl.mzlnk.evolution.api.model.location.Location;

public interface WorldMoveStrategy {

    Location add(Location v1, Location v2);

}
