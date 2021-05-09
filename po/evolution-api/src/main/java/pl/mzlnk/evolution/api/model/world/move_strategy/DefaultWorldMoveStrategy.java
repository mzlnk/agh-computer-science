package pl.mzlnk.evolution.api.model.world.move_strategy;

import pl.mzlnk.evolution.api.model.location.Location;

public class DefaultWorldMoveStrategy implements WorldMoveStrategy {

    @Override
    public Location add(Location v1, Location v2) {
        return v1.add(v2);
    }

}
