package pl.mzlnk.evolution.api.scheduler;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import pl.mzlnk.evolution.api.controller.socket.EvolutionLiveController;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.repository.EvolutionRepository;

@Component
public class EvolutionScheduler {

    @Autowired
    private EvolutionRepository evolutionRepository;

    @Autowired
    private EvolutionLiveController evolutionLiveController;

    @Scheduled(fixedRate = 50)
    public void evolutionNextDay() {
        evolutionRepository
                .findAll()
                .forEach(Evolution::nextDay);
        evolutionLiveController.sendEvolutionMapOverview();
    }

    @Scheduled(fixedRate = 1000L, initialDelay = 10L)
    public void sendEvolutionStatistics() {
        evolutionLiveController.sendEvolutionStatistics();
    }

    @Scheduled(fixedRate = 1000L, initialDelay = 20L)
    public void sendEvolutionEntitiesOverview() {
        evolutionLiveController.sendEvolutionEntitiesOverview();
    }

    @Scheduled(fixedRate = 1000)
    public void sendEvolutionProperties() {
        evolutionLiveController.sendEvolutionProperties();
    }

    @Scheduled(fixedRate = 1000)
    public void sendEvolutionAllProperties() {
        evolutionLiveController.sendAllEvolutionProperties();
    }

}
