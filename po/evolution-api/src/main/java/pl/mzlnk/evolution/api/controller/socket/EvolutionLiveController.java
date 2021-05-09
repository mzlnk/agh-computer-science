package pl.mzlnk.evolution.api.controller.socket;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Controller;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.model.evolution.EvolutionEntitiesOverview;
import pl.mzlnk.evolution.api.model.evolution.EvolutionMapOverview;
import pl.mzlnk.evolution.api.model.evolution.EvolutionStatistics;
import pl.mzlnk.evolution.api.repository.EvolutionRepository;

import java.util.stream.Collectors;

@Controller
public class EvolutionLiveController {

    private static final String SOCKET_EVOLUTION_DAY_URL = "/live/evolution/id/{id}/day";
    private static final String SOCKET_EVOLUTION_ENTITIES_URL = "/live/evolution/id/{id}/entities";
    private static final String SOCKET_EVOLUTION_STATS_URL = "/live/evolution/id/{id}/statistics";
    private static final String SOCKET_EVOLUTION_PROPERTIES_URL = "/live/evolution/id/{id}/properties";
    private static final String SOCKET_EVOLUTION_ALL_PROPERTIES_URL = "/live/evolution/all/properties";

    @Autowired
    private EvolutionRepository evolutionRepository;

    @Autowired
    private SimpMessagingTemplate template;

    public void sendEvolutionMapOverview() {
        evolutionRepository
                .findAll()
                .forEach(evolution -> {
                    EvolutionMapOverview mapOverview = EvolutionMapOverview.fromEvolution(evolution);
                    template.convertAndSend(SOCKET_EVOLUTION_DAY_URL.replace("{id}", evolution.getUuid().toString()), mapOverview);
                });
    }

    public void sendEvolutionEntitiesOverview() {
        evolutionRepository
                .findAll()
                .forEach(evolution -> {
                    EvolutionEntitiesOverview entitiesOverview = EvolutionEntitiesOverview.fromEvolution(evolution);
                    template.convertAndSend(SOCKET_EVOLUTION_ENTITIES_URL.replace("{id}", evolution.getUuid().toString()), entitiesOverview);
                });
    }

    public void sendEvolutionProperties() {
        evolutionRepository
                .findAll()
                .forEach(sm -> {
                    template.convertAndSend(SOCKET_EVOLUTION_PROPERTIES_URL.replace("{id}", sm.getUuid().toString()), sm.getProperties());
                });
    }

    public void sendAllEvolutionProperties() {
        var allProperties = evolutionRepository
                .findAll()
                .stream()
                .map(Evolution::getProperties)
                .collect(Collectors.toList());

        template.convertAndSend(SOCKET_EVOLUTION_ALL_PROPERTIES_URL, allProperties);
    }

    public void sendEvolutionStatistics() {
        evolutionRepository
                .findAll()
                .forEach(sm -> {
                    template.convertAndSend(SOCKET_EVOLUTION_STATS_URL.replace("{id}", sm.getUuid().toString()), EvolutionStatistics.fromEvolution(sm));
                });
    }

}
