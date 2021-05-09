package pl.mzlnk.po.lab7.map.impl;

import org.junit.jupiter.api.Test;
import pl.mzlnk.po.lab7.map.element.Animal;
import pl.mzlnk.po.lab7.map.element.Grass;
import pl.mzlnk.po.lab7.map.element.Vector2D;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AbstractWorldMapTest {

    @Test
    void findElements() {
        GrassFieldMap map = new GrassFieldMap(10);
        map.place(new Animal(map, new Vector2D(4, 4)));
        map.place(new Animal(map, new Vector2D(4, 2)));

        assertEquals(2, map.findElementsByType(Animal.class).size());
        assertEquals(10, map.findElementsByType(Grass.class).size());
    }

    @Test
    void testToString() {
        RectangularMap map = new RectangularMap(4, 4);
        map.place(new Animal(map, new Vector2D(2, 2)));
        map.place(new Animal(map, new Vector2D(0, 0)));
        System.out.println(map.toString());
    }

}