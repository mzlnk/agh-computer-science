package pl.mzlnk.evolution.api.repository.impl;

import org.springframework.stereotype.Repository;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.repository.EvolutionRepository;

import java.util.*;

@Repository
public class EvolutionRepositoryImpl implements EvolutionRepository {

    private Map<UUID, Evolution> evolutions = new HashMap<>();

    @Override
    public List<Evolution> findAll() {
        return new ArrayList<>(evolutions.values());
    }

    @Override
    public Optional<Evolution> findByUuid(UUID uuid) {
        return Optional.ofNullable(evolutions.get(uuid));
    }

    @Override
    public void createOrUpdateSimulation(Evolution simulation) {
        evolutions.put(simulation.getUuid(), simulation);
    }

    @Override
    public void removeSimulation(UUID uuid) {
        evolutions.remove(uuid);
    }

}
