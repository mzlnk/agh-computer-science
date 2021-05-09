package pl.mzlnk.po.lab5.map.impl;

import pl.mzlnk.po.lab5.dto.Vector2D;
import pl.mzlnk.po.lab5.map.IMapElement;
import pl.mzlnk.po.lab5.map.IWorldMap;
import pl.mzlnk.po.lab5.map.utils.MapVisualizer;
import pl.mzlnk.po.lab5.utils.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.stream.Collectors;

public abstract class AbstractWorldMap implements IWorldMap {

    protected List<IMapElement> mapElements = new ArrayList<>();

    public AbstractWorldMap() {
    }

    protected abstract Pair<Vector2D, Vector2D> getMapBounds();

    @Override
    public boolean place(IMapElement element) {
        if (isOccupied(element.getPosition())) {
            return false;
        }

        return mapElements.add(element);
    }

    @Override
    public boolean remove(IMapElement element) {
        for (int i = 0; i < mapElements.size(); i++) {
            if (mapElements.get(i) == element) {
                mapElements.remove(i);
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean canMoveTo(Vector2D position) {
        return !isOccupied(position);
    }

    @Override
    public boolean isOccupied(Vector2D position) {
        return mapElements
                .stream()
                .anyMatch(elem -> elem.getPosition().equals(position));
    }

    @Override
    public Optional<IMapElement> objectAt(Vector2D position) {
        return mapElements
                .stream()
                .filter(elem -> elem.getPosition().equals(position))
                .findAny();
    }

    @Override
    public List<IMapElement> findAllMapElements() {
        return mapElements;
    }

    @Override
    public String toString() {
        Pair<Vector2D, Vector2D> mapBounds = getMapBounds();
        System.out.println("elements: " + mapElements.size());
        System.out.println("LL: " + mapBounds.getKey().toString());
        System.out.println("UR: " + mapBounds.getValue().toString());
        return new MapVisualizer(this).draw(mapBounds.getKey(), mapBounds.getValue());
    }

    protected <T extends IMapElement> List<T> findElementsByType(Class<T> clazz) {
        return mapElements
                .stream()
                .filter(e -> e.getClass().equals(clazz))
                .map(e -> (T) e)
                .collect(Collectors.toList());
    }

}
