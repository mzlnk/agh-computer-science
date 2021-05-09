package pl.mzlnk.po.lab5.map.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import pl.mzlnk.po.lab5.dto.Animal;
import pl.mzlnk.po.lab5.dto.Grass;
import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;
import pl.mzlnk.po.lab5.map.IWorldMap;
import pl.mzlnk.po.lab5.utils.OptionParser;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

class GrassFieldMapTest {

    IWorldMap map;
    Animal animal1, animal2;

    @BeforeEach
    void init() {
        map = new GrassFieldMap(10);
        animal1 = new Animal(map, new Vector2D(3, 4));
        animal2 = new Animal(map, new Vector2D(-3, 9));
    }

    @Test
    void canMoveTo() {
        map.place(animal1);
        map.place(animal2);

        assumeTrue(((GrassFieldMap) map).findElementsByType(Animal.class).size() == 2, "Could not place two animals. Aborting...");

        assertTrue(map.canMoveTo(new Vector2D(-2, 0)));
        assertTrue(map.canMoveTo(new Vector2D(20, 20)));
        assertFalse(map.canMoveTo(new Vector2D(3, 4)));
        assertFalse(map.canMoveTo(new Vector2D(-3, 9)));
    }

    @Test
    void place() {
        assertTrue(map.place(new Animal(map, new Vector2D(-5, -5))));
        assertTrue(map.place(new Animal(map, new Vector2D(-5, -15))));
        assertFalse(map.place(new Animal(map, new Vector2D(-5, -5))));
    }

    @Test
    void remove() {
        map.place(animal1);

        assertTrue(map.remove(animal1));
        assertFalse(map.remove(new Animal(map, new Vector2D(-2, -10))));
    }

    @RepeatedTest(10)
    void run() {
        MoveDirection[] directions = OptionParser.parse(new String[]{"f", "b", "r", "f", "l", "b", "f", "f"});

        map.place(animal1);
        map.place(animal2);

        assumeTrue(((GrassFieldMap) map).findElementsByType(Animal.class).size() == 2, "Could not place two animals. Aborting...");

        map.run(directions);

        Animal endAnimal1 = new Animal(map, new Vector2D(-3, 9));
        Animal endAnimal2 = new Animal(map, new Vector2D(3, 6));

        assertEquals(map.objectAt(new Vector2D(-3, 9)).orElse(null), endAnimal1);
        assertEquals(map.objectAt(new Vector2D(3, 6)).orElse(null), endAnimal2);
    }

    @RepeatedTest(10)
    void generateGrassFields() {
        AbstractWorldMap map2 = new GrassFieldMap(15);
        assertEquals(15, map2.findElementsByType(Grass.class).size());
        assertEquals(15, map2.findAllMapElements().size());
    }

    @Test
    void isOccupied() {
        map.findAllMapElements()
                .forEach(element -> assertTrue(map.isOccupied(element.getPosition())));

        assertFalse(map.isOccupied(new Vector2D(-1, -1)));
        assertFalse(map.isOccupied(new Vector2D(201, -10)));
    }

    @Test
    void objectAt() {
        map.place(animal1);
        map.place(animal2);

        assertTrue(map.objectAt(new Vector2D(3, 4)).isPresent());
        assertTrue(map.objectAt(new Vector2D(-3, 9)).isPresent());
        assertFalse(map.objectAt(new Vector2D(-1000, -1000)).isPresent());
        assertFalse(map.objectAt(new Vector2D(-1000, 1000)).isPresent());
    }

}