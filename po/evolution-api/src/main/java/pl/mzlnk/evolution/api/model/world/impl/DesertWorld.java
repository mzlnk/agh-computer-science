package pl.mzlnk.evolution.api.model.world.impl;

import pl.mzlnk.evolution.api.model.world.WorldProperties;
import pl.mzlnk.evolution.api.model.world.move_strategy.WrapWorldEdgesMoveStrategy;

public class DesertWorld extends BaseWorld {

    public DesertWorld(WorldProperties worldProperties) {
        super(worldProperties);

        this.worldMoveStrategy = new WrapWorldEdgesMoveStrategy(worldProperties);
    }

}
