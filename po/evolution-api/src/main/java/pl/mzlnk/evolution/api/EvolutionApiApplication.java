package pl.mzlnk.evolution.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;
import pl.mzlnk.evolution.api.config.EvolutionConfig;
import pl.mzlnk.evolution.api.model.evolution.Evolution;
import pl.mzlnk.evolution.api.repository.EvolutionRepository;
import pl.mzlnk.evolution.api.utils.TerminalUtil;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

@SpringBootApplication
@EnableScheduling
public class EvolutionApiApplication implements ApplicationRunner {

    public static final Logger logger = LoggerFactory.getLogger(EvolutionApiApplication.class);

    @Autowired
    private EvolutionRepository evolutionRepository;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.println(TerminalUtil.welcomeTitle());
        System.out.println("Starting Evolution 2.0...");
        System.out.println("Do you want to load configuration? (y/N)");

        List<String> argsList = new ArrayList<>();
        if (sc.next().equalsIgnoreCase("y")) {
            System.out.println("Please enter path to file with configuration:");
            System.out.println("(Current directory: " + (new File("").getAbsolutePath()) + ")");
            String path = sc.next();
            argsList.add(path);
        }

        SpringApplication.run(EvolutionApiApplication.class, argsList.toArray(String[]::new));
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        if (args.getSourceArgs().length < 1) {
            return;
        }

        File configFile = new File(args.getSourceArgs()[0]);
        EvolutionConfig.fromFile(configFile)
                .ifPresent(config -> {
                    config.getEvolutions()
                            .forEach(props -> evolutionRepository.createOrUpdateSimulation(Evolution.newInstance(props)));
                });
        logger.info("Loaded: " + evolutionRepository.findAll().size() + " Evolutions");
    }

}
