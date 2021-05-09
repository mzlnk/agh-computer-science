package pl.mzlnk.po.lab6.map.impl;

import pl.mzlnk.po.lab6.dto.Vector2D;
import pl.mzlnk.po.lab6.map.IMapElement;
import pl.mzlnk.po.lab6.map.IWorldMap;
import pl.mzlnk.po.lab6.map.utils.MapVisualizer;
import pl.mzlnk.po.lab6.utils.Pair;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractWorldMap implements IWorldMap {

    protected Map<Vector2D, IMapElement> mapElements = new LinkedHashMap<>();

    public AbstractWorldMap() {
    }

    protected abstract Pair<Vector2D, Vector2D> getMapBounds();

    @Override
    public boolean place(IMapElement element) {
        if (isOccupied(element.getPosition())) {
            throw new IllegalArgumentException("Cannot place element - the other one is already on given position");
        }

        mapElements.put(element.getPosition(), element);
        return true;
    }

    @Override
    public boolean remove(IMapElement element) {
        return mapElements.remove(element.getPosition()) != null;
    }

    @Override
    public boolean canMoveTo(Vector2D position) {
        return !isOccupied(position);
    }

    @Override
    public boolean isOccupied(Vector2D position) {
        return mapElements.containsKey(position);
    }

    @Override
    public Optional<IMapElement> objectAt(Vector2D position) {
        return Optional.ofNullable(mapElements.get(position));
    }

    @Override
    public List<IMapElement> findAllMapElements() {
        return new ArrayList<>(mapElements.values());
    }

    @Override
    public String toString() {
        Pair<Vector2D, Vector2D> mapBounds = getMapBounds();
        return new MapVisualizer(this).draw(mapBounds.getKey(), mapBounds.getValue());
    }

    protected <T extends IMapElement> List<T> findElementsByType(Class<T> clazz) {
        return mapElements
                .values()
                .stream()
                .filter(e -> e.getClass().equals(clazz))
                .map(e -> (T) e)
                .collect(Collectors.toList());
    }

}
