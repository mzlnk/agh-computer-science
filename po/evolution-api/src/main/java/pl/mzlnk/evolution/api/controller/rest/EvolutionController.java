package pl.mzlnk.evolution.api.controller.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.model.evolution.EvolutionProperties;
import pl.mzlnk.evolution.api.service.EvolutionService;

@RestController
@RequestMapping("/evolution")
public class EvolutionController {

    @Autowired
    private EvolutionService evolutionService;

    @PostMapping("/add")
    public void addEvolution(@RequestBody EvolutionProperties properties,
                             @RequestParam String token) {
        // todo: add token verification
        Evolution evolution = Evolution.newInstance(properties);
        evolutionService.createOrUpdateSimulation(evolution);
    }


}
