package pl.mzlnk.evolution.api.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.repository.EvolutionRepository;
import pl.mzlnk.evolution.api.service.EvolutionService;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
public class EvolutionServiceImpl implements EvolutionService {

    @Autowired
    private EvolutionRepository evolutionRepository;

    @Override
    public List<Evolution> findAll() {
        return evolutionRepository.findAll();
    }

    @Override
    public Optional<Evolution> findByUuid(UUID uuid) {
        return evolutionRepository.findByUuid(uuid);
    }

    @Override
    public void createOrUpdateSimulation(Evolution simulation) {
        evolutionRepository.createOrUpdateSimulation(simulation);
    }

    @Override
    public void removeSimulation(UUID uuid) {
        evolutionRepository.removeSimulation(uuid);
    }
}
