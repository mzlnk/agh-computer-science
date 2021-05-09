package pl.mzlnk.evolution.api.repository;

import pl.mzlnk.evolution.api.model.evolution.Evolution;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface EvolutionRepository {

    List<Evolution> findAll();
    Optional<Evolution> findByUuid(UUID uuid);
    void createOrUpdateSimulation(Evolution simulation);
    void removeSimulation(UUID uuid);

}
