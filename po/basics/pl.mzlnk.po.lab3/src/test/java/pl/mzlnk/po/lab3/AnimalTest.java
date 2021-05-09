package pl.mzlnk.po.lab3;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static pl.mzlnk.po.lab3.MapDirection.*;
import static pl.mzlnk.po.lab3.MoveDirection.*;

public class AnimalTest {

    private Animal animal1;
    private Animal animal2;
    private Vector2D startVector;

    @Before
    public void setUp() {
        animal1 = new Animal();
        animal2 = new Animal();
        startVector = new Vector2D(2, 2);
    }

    @Test
    public void orientationTest() {
        assertEquals(NORTH, animal1.getMapDirection());
        assertEquals(NORTH, animal2.getMapDirection());
    }

    @Test
    public void move() {
        assertEquals(startVector, animal1.getPosition());

        animal1.move(FORWARD);
        assertEquals(new Vector2D(2, 3), animal1.getPosition());

        animal1.move(BACKWARD);
        assertEquals(new Vector2D(2, 2), animal1.getPosition());

        animal1.move(LEFT);
        assertEquals(WEST, animal1.getMapDirection());

        animal1.move(RIGHT);
        assertEquals(NORTH, animal1.getMapDirection());

        animal1.move(LEFT, FORWARD, FORWARD, LEFT, FORWARD);
        assertEquals(SOUTH, animal1.getMapDirection());
        assertEquals(new Vector2D(0, 1), animal1.getPosition());
    }

    @Test
    public void moveToBounds() {

        assertEquals(startVector, animal1.getPosition());

        animal1.move(FORWARD, FORWARD, FORWARD, FORWARD, FORWARD);
        assertEquals(new Vector2D(2, 4), animal1.getPosition());

        animal2.move(RIGHT, FORWARD, FORWARD, FORWARD, FORWARD);
        assertEquals(new Vector2D(4, 2), animal2.getPosition());
    }

}