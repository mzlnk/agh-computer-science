package pl.mzlnk.po.lab2;

import org.junit.Test;

import static org.junit.Assert.*;
import static pl.mzlnk.po.lab2.MapDirection.*;

public class MapDirectionTest {

    @Test
    public void next() {
        assertEquals(NORTH, EAST.previous());
        assertEquals(EAST, SOUTH.previous());
        assertEquals(SOUTH, WEST.previous());
        assertEquals(WEST, NORTH.previous());
    }

    @Test
    public void previous() {
        assertEquals(NORTH, WEST.next());
        assertEquals(EAST, NORTH.next());
        assertEquals(SOUTH, EAST.next());
        assertEquals(WEST, SOUTH.next());
    }
}