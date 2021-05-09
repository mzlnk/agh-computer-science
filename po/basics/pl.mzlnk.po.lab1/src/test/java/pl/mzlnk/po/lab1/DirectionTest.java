package pl.mzlnk.po.lab1;

import org.junit.Test;

import static org.junit.Assert.*;

public class DirectionTest {

    @Test
    public void getByShortcut() {
        assertEquals(Direction.FORWARD, Direction.getByShortcut("f").orElseThrow());
        assertEquals(Direction.BACKWARD, Direction.getByShortcut("b").orElseThrow());
        assertEquals(Direction.LEFT, Direction.getByShortcut("l").orElseThrow());
        assertEquals(Direction.RIGHT, Direction.getByShortcut("r").orElseThrow());
    }

}