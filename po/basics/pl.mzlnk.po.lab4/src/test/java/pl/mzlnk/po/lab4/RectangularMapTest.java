package pl.mzlnk.po.lab4;

import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

public class RectangularMapTest {

    private String[] args;

    private IWorldMap map;
    private Animal animal1;
    private Animal animal2;
    private Animal animal3;

    @Before
    public void setUp() {
        args = new String[] {"f", "b", "r", "l", "f", "f", "r", "r", "f", "f", "f", "f", "f", "f", "f,", "f"};

        map = new RectangularMap(5, 5);
        animal1 = new Animal(map, new Vector2D(1, 1));
        animal2 = new Animal(map, new Vector2D(2, 2));
        animal3 = new Animal(map, new Vector2D(2, 2));
    }

    @Test
    public void canMoveTo() {
        map.place(animal1);

        assertTrue(map.canMoveTo(new Vector2D(0, 0)));
        assertFalse(map.canMoveTo(new Vector2D(-3, -1)));
        assertFalse(map.canMoveTo(new Vector2D(1, 1)));
    }

    @Test
    public void place() {
        assertTrue(map.place(animal1));
        assertTrue(map.place(animal2));
        assertFalse(map.place(animal3));
    }

    @Test
    public void run() {
        map.place(animal1);
        map.place(animal2);

        map.run(OptionParser.parse(args));

        Animal animalEnd1 = new Animal(map, new Vector2D(1, 4), MapDirection.NORTH);
        Animal animalEnd2 = new Animal(map, new Vector2D(2, 0), MapDirection.SOUTH);

        assertEquals(map.objectAt(new Vector2D(1, 4)), animalEnd1);
        assertEquals(map.objectAt(new Vector2D(2, 0)), animalEnd2);
    }

    @Test
    public void isOccupied() {
        map.place(animal1);
        map.place(animal2);

        assertTrue(map.isOccupied(new Vector2D(1, 1)));
        assertTrue(map.isOccupied(new Vector2D(2, 2)));
        assertFalse(map.isOccupied(new Vector2D(3, 1)));
        assertFalse(map.isOccupied(new Vector2D(1, 5)));
    }

    @Test
    public void objectAt() {
        assertNull(map.objectAt(new Vector2D(1, 1)));
        map.place(animal1);
        assertNotNull(map.objectAt(new Vector2D(1, 1)));
    }

    @Test
    public void testToString() {
        System.out.println(map.toString());
    }
}