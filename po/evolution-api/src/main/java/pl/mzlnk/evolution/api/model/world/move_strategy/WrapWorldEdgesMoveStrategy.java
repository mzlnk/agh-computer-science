package pl.mzlnk.evolution.api.model.world.move_strategy;

import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.WorldProperties;

import static java.lang.Math.abs;

public class WrapWorldEdgesMoveStrategy implements WorldMoveStrategy {

    private WorldProperties metrics;

    public WrapWorldEdgesMoveStrategy(WorldProperties metrics) {
        this.metrics = metrics;
    }

    @Override
    public Location add(Location v1, Location v2) {
        Location def = v1.add(v2);

        int x = def.x >= 0 ? (def.x < metrics.mapSize ? def.x : (abs(def.x - metrics.mapSize + 1))) : (metrics.mapSize - abs(def.x));
        int y = def.y >= 0 ? (def.y < metrics.mapSize ? def.y : (abs(def.y - metrics.mapSize + 1))) : (metrics.mapSize - abs(def.y));

        return new Location(x, y);
    }

}
