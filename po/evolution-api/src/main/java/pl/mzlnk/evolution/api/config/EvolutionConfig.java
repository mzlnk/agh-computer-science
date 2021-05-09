package pl.mzlnk.evolution.api.config;

import com.google.gson.Gson;
import lombok.Data;
import pl.mzlnk.evolution.api.model.evolution.EvolutionProperties;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static pl.mzlnk.evolution.api.EvolutionApiApplication.logger;

@Data
public class EvolutionConfig {

    public static Optional<EvolutionConfig> fromFile(File file) {
        try (BufferedReader br = new BufferedReader(new FileReader(file))) {
            EvolutionConfig config = new Gson().fromJson(br, EvolutionConfig.class);
            if (config.isValid()) {
                return Optional.of(config);
            } else {
                logger.warn("Config file - " + file.getName() + " is not valid");
            }
        } catch (IOException e) {
            logger.warn("Could not read file - " + file.getName());
        }
        return Optional.empty();
    }

    private List<EvolutionProperties> evolutions = new ArrayList<>();

    private boolean isValid() {
        return evolutions
                .stream()
                .allMatch(EvolutionProperties::isValid);
    }

}
