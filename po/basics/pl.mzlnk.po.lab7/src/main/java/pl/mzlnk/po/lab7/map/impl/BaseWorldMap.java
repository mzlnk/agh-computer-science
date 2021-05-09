package pl.mzlnk.po.lab7.map.impl;

import pl.mzlnk.po.lab7.enums.MoveDirection;
import pl.mzlnk.po.lab7.map.PositionChangedObserver;
import pl.mzlnk.po.lab7.map.WorldMap;
import pl.mzlnk.po.lab7.map.element.MapElement;
import pl.mzlnk.po.lab7.map.element.Vector2D;
import pl.mzlnk.po.lab7.map.utils.MapVisualizer;
import pl.mzlnk.po.lab7.utils.Pair;

import java.util.*;
import java.util.stream.Collectors;

public abstract class BaseWorldMap implements WorldMap, PositionChangedObserver {

    protected Map<Vector2D, MapElement> mapElementsMap = new LinkedHashMap<>();
    protected List<MapElement> mapElementList = new ArrayList<>();

    protected MapBoundary mapBoundary = new MapBoundary();

    public BaseWorldMap() {
    }

    @Override
    public boolean canMoveTo(Vector2D position) {
        return !isOccupied(position);
    }

    @Override
    public boolean place(MapElement element) {
        if (isOccupied(element.getPosition())) {
            throw new IllegalArgumentException("Cannot place element - the other one is already on given position");
        }

        mapElementsMap.put(element.getPosition(), element);
        mapElementList.add(element);
        mapBoundary.addElement(element.getPosition(), element);
        return true;
    }

    @Override
    public boolean remove(MapElement element) {
        mapElementsMap.remove(element.getPosition());
        mapElementList.remove(element);
        mapBoundary.removeElement(element.getPosition());

        return true;
    }

    @Override
    public void run(MoveDirection[] directions) {
        List<MapElement> elements = findAllMapElements();
        for (int i = 0; i < elements.size(); i++) {
            elements.get(i & elements.size()).move(directions[i]);
        }
    }

    @Override
    public boolean isOccupied(Vector2D position) {
        return mapElementsMap.containsKey(position);
    }

    @Override
    public Optional<MapElement> objectAt(Vector2D position) {
        return Optional.ofNullable(mapElementsMap.get(position));
    }

    @Override
    public List<MapElement> findAllMapElements() {
        return new ArrayList<>(mapElementList);
    }

    @Override
    public String toString() {
        Pair<Vector2D, Vector2D> mapBounds = mapBoundary.getBoundaries();
        return new MapVisualizer(this).draw(mapBounds.getKey(), mapBounds.getValue());
    }

    @Override
    public void positionChanged(Vector2D oldPosition, Vector2D newPosition) {
        mapBoundary.positionChanged(oldPosition, newPosition);
        MapElement element = mapElementsMap.get(oldPosition);
        mapElementsMap.remove(oldPosition);
        mapElementsMap.put(newPosition, element);
    }

    protected <T extends MapElement> List<T> findElementsByType(Class<T> clazz) {
        return mapElementList
                .stream()
                .filter(e -> e.getClass().equals(clazz))
                .map(e -> (T) e)
                .collect(Collectors.toList());
    }

}
