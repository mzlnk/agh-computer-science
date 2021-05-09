package pl.mzlnk.po.lab2;

import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

public class Vector2DTest {

    private static Vector2D vector1, vector2, vector3, vector4, vector5;

    @BeforeClass
    public static void setUp() {
        vector1 = new Vector2D(1, 3);
        vector2 = new Vector2D(3, -2);
        vector3 = new Vector2D(-1, -3);
        vector4 = new Vector2D(4, 1);
        vector5 = new Vector2D(1, 3);
    }

    @Test
    public void precedes() {
        assertTrue(vector3.precedes(vector1));
        assertFalse(vector1.precedes(vector4));
    }

    @Test
    public void follows() {
        assertTrue(vector1.follows(vector3));
        assertFalse(vector4.follows(vector1));
    }

    @Test
    public void upperRight() {
        assertEquals(vector1, vector1.upperRight(vector3));
    }

    @Test
    public void lowerLeft() {
        assertEquals(vector3, vector1.lowerLeft(vector3));
    }

    @Test
    public void add() {
        assertEquals(vector4, vector1.add(vector2));
    }

    @Test
    public void subtract() {
        assertEquals(vector1, vector4.subtract(vector2));
    }

    @Test
    public void opposite() {
        assertEquals(vector3, vector1.opposite());
    }

    @Test
    public void testEquals() {
        assertEquals(vector1, vector5);
        assertNotEquals(vector1, vector2);
    }

    @Test
    public void testToString() {
        assertEquals("(3,-2)", vector2.toString());
        assertEquals("(4, 1", vector4.toString());
    }

}