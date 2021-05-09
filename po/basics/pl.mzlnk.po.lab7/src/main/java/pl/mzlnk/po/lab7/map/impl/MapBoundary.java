package pl.mzlnk.po.lab7.map.impl;

import pl.mzlnk.po.lab7.map.PositionChangedObserver;
import pl.mzlnk.po.lab7.map.element.MapElement;
import pl.mzlnk.po.lab7.map.element.Vector2D;
import pl.mzlnk.po.lab7.utils.Pair;

import java.util.*;

public class MapBoundary implements PositionChangedObserver {

    private Map<Vector2D, MapElement> elements = new HashMap<>();
    private TreeMap<Vector2D, MapElement> xAxisElements = new TreeMap<>(MapBoundary.Comparator::compareByXAxis);
    private TreeMap<Vector2D, MapElement> yAxisElements = new TreeMap<>(MapBoundary.Comparator::compareByYAxis);

    @Override
    public void positionChanged(Vector2D oldPosition, Vector2D newPosition) {
        MapElement element = elements.get(oldPosition);
        this.removeElement(oldPosition);
        this.addElement(newPosition, element);
    }

    public void addElement(Vector2D position, MapElement element) {
        elements.put(position, element);
        xAxisElements.put(position, element);
        yAxisElements.put(position, element);
    }

    public void removeElement(Vector2D position) {
        elements.remove(position);
        xAxisElements.remove(position);
        yAxisElements.remove(position);
    }

    public Pair<Vector2D, Vector2D> getBoundaries() {
        BoundCoordinates coordinates = new BoundCoordinates();

        try {
            coordinates.minX = xAxisElements.firstKey().x;
            coordinates.maxX = xAxisElements.lastKey().x;
            System.out.println(1);
        } catch (NoSuchElementException ignored) {}

        try {
            coordinates.minY = yAxisElements.firstKey().y;
            coordinates.maxY = yAxisElements.lastKey().y;
            System.out.println(2);
        } catch (NoSuchElementException ignored) {}

        return new Pair<>(new Vector2D(coordinates.minX, coordinates.minY), new Vector2D(coordinates.maxX, coordinates.maxY));
    }

    private static class BoundCoordinates {

        private int minX = 0;
        private int minY = 0;
        private int maxX = 0;
        private int maxY = 0;

    }

    private static class Comparator {

        private static int compareByXAxis(Vector2D v1, Vector2D v2) {
            if (v1.x > v2.x) return 1;
            if (v1.x < v2.x) return -1;

            if (v1.y > v2.y) return 1;
            if (v1.y < v2.y) return -1;

            return 0;
        }

        private static int compareByYAxis(Vector2D v1, Vector2D v2) {
            if (v1.y > v2.y) return 1;
            if (v1.y < v2.y) return -1;

            if (v1.x > v2.x) return 1;
            if (v1.x < v2.x) return -1;

            return 0;
        }

    }

}
