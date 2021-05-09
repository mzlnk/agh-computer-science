package pl.mzlnk.evolution.api.model.world.enums;

import lombok.AllArgsConstructor;
import pl.mzlnk.evolution.api.model.location.Location;
import pl.mzlnk.evolution.api.model.world.WorldProperties;

public enum WorldSector {

    WILDERNESS,
    JUNGLE;

    public static WorldSector[] ALL = {WILDERNESS, JUNGLE};

    @AllArgsConstructor
    public static class Util {

        private Location jungleLowerLeft;
        private Location jungleUpperRight;

        public Util(WorldProperties metrics) {
            this.jungleLowerLeft = new Location((metrics.mapSize - metrics.jungleSize) / 2, (metrics.mapSize - metrics.jungleSize) / 2);
            this.jungleUpperRight = jungleLowerLeft.add(new Location(metrics.jungleSize, metrics.jungleSize));
        }

        public WorldSector fromLocation(Location position) {
            if (position.between(jungleLowerLeft, jungleUpperRight)) {
                return JUNGLE;
            }
            return WILDERNESS;
        }

    }

}
