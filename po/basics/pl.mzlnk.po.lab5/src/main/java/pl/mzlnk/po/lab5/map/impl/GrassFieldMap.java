package pl.mzlnk.po.lab5.map.impl;

import pl.mzlnk.po.lab5.dto.Animal;
import pl.mzlnk.po.lab5.dto.Grass;
import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.enums.MoveDirection;
import pl.mzlnk.po.lab5.map.IMapElement;
import pl.mzlnk.po.lab5.utils.Pair;

import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GrassFieldMap extends AbstractWorldMap {

    private static Random r = new Random();

    private int targetGrassFields;

    public GrassFieldMap(int targetGrassFields) {
        this.targetGrassFields = targetGrassFields;
        generateGrassFields();
    }

    @Override
    public void run(MoveDirection[] directions) {
        List<Animal> animals = findElementsByType(Animal.class);

        AtomicInteger i = new AtomicInteger(0);
        Stream.of(directions)
                .forEach(moveDirection -> {
                    animals.get(i.getAndIncrement() % animals.size()).move(moveDirection);
                });
    }

    @Override
    public boolean remove(IMapElement element) {
        boolean result = super.remove(element);
        generateGrassFields();

        return result;
    }

    @Override
    protected Pair<Vector2D, Vector2D> getMapBounds() {
        IntSummaryStatistics xBounds = mapElements
                .stream()
                .map(IMapElement::getPosition)
                .collect(Collectors.summarizingInt(Vector2D::getX));

        IntSummaryStatistics yBounds = mapElements
                .stream()
                .map(IMapElement::getPosition)
                .collect(Collectors.summarizingInt(Vector2D::getY));

        return new Pair<>(new Vector2D(xBounds.getMin(), yBounds.getMin()), new Vector2D(xBounds.getMax(), yBounds.getMax()));
    }

    private void generateGrassFields() {
        final Vector2D lowerLeft = new Vector2D(0, 0);
        final Vector2D upperRight = new Vector2D((int) Math.sqrt(targetGrassFields * 10), (int) Math.sqrt(targetGrassFields * 10));

        final int currentGrassFieldsAmount = findElementsByType(Grass.class).size();

        if (Vector2D.Utils.countFields(lowerLeft, upperRight) < (targetGrassFields - currentGrassFieldsAmount) + mapElements.size()) {
            throw new IllegalArgumentException("More expected grass fields to be generated than applicable fields");
        }

        for (int i = currentGrassFieldsAmount; i < targetGrassFields; i++) {
            Vector2D pos;
            do {
                pos = new Vector2D(r.nextInt(upperRight.x + 1), r.nextInt(upperRight.y + 1));
            } while (this.isOccupied(pos));
            mapElements.add(new Grass(this, pos));
        }
    }

}
