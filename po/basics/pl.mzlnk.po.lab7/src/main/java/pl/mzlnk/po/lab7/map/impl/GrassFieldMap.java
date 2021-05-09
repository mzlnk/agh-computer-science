package pl.mzlnk.po.lab7.map.impl;

import pl.mzlnk.po.lab7.enums.MoveDirection;
import pl.mzlnk.po.lab7.map.element.Animal;
import pl.mzlnk.po.lab7.map.element.Grass;
import pl.mzlnk.po.lab7.map.element.MapElement;
import pl.mzlnk.po.lab7.map.element.Vector2D;

import java.util.List;
import java.util.Random;

public class GrassFieldMap extends BaseWorldMap {

    private static Random r = new Random();

    private int targetGrassFields;

    public GrassFieldMap(int targetGrassFields) {
        this.targetGrassFields = targetGrassFields;
        generateGrassFields();
    }

    @Override
    public void run(MoveDirection[] directions) {
        List<Animal> animals = findElementsByType(Animal.class);
        for (int i = 0; i < animals.size(); i++) {
            animals.get(i % animals.size()).move(directions[i]);
        }
    }

    @Override
    public boolean remove(MapElement element) {
        boolean result = super.remove(element);
        generateGrassFields();

        return result;
    }

    private void generateGrassFields() {
        final Vector2D lowerLeft = new Vector2D(0, 0);
        final Vector2D upperRight = new Vector2D((int) Math.sqrt(targetGrassFields * 10), (int) Math.sqrt(targetGrassFields * 10));

        final int currentGrassFieldsAmount = findElementsByType(Grass.class).size();

        if (Vector2D.Utils.countFields(lowerLeft, upperRight) < (targetGrassFields - currentGrassFieldsAmount) + mapElementsMap.size()) {
            throw new IllegalArgumentException("More expected grass fields to be generated than applicable fields");
        }

        for (int i = currentGrassFieldsAmount; i < targetGrassFields; i++) {
            Vector2D pos;
            do {
                pos = new Vector2D(r.nextInt(upperRight.x + 1), r.nextInt(upperRight.y + 1));
            } while (this.isOccupied(pos));
            mapElementsMap.put(pos, new Grass(this, pos));
        }
    }

}
