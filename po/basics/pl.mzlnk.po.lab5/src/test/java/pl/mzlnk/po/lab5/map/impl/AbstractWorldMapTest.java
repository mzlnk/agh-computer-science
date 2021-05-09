package pl.mzlnk.po.lab5.map.impl;

import org.junit.jupiter.api.Test;
import pl.mzlnk.po.lab5.dto.Animal;
import pl.mzlnk.po.lab5.dto.Grass;
import pl.mzlnk.po.lab5.dto.Vector2D;

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

}