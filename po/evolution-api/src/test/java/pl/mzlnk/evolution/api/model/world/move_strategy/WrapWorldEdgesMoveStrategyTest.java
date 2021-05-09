package pl.mzlnk.evolution.api.model.world.move_strategy;

import org.junit.jupiter.api.Test;

import static java.lang.Math.abs;
import static org.junit.jupiter.api.Assertions.*;

class WrapWorldEdgesMoveStrategyTest {


    @Test
    void addTest() {
        int mapSize = 30;

        int x = 30;
        int y = 30;

        System.out.println(x >= 0 ? (x < mapSize ? x : (abs(x - mapSize))) : (mapSize - abs(x)));
        System.out.println(y >= 0 ? (y < mapSize ? y : (abs(y - mapSize))) : (mapSize - abs(y)));
    }

}