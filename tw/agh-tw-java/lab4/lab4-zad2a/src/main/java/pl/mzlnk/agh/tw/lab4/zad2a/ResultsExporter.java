package pl.mzlnk.agh.tw.lab4.zad2a;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

@Slf4j
public class ResultsExporter {

    public static void exportResults(Map<Integer, TimingResult> producerResults,
                                     Map<Integer, TimingResult> consumerResults,
                                     String filename) {
        Gson gson = new GsonBuilder()
                .setPrettyPrinting()
                .create();

        var results = new Results(producerResults, consumerResults);
        String json = gson.toJson(results);
        File dir = new File("lab4-zad2");
        dir.mkdirs();

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(new File(dir, filename + ".json")))) {
            writer.write(json);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @RequiredArgsConstructor
    private static class Results {
        final Map<Integer, TimingResult> producerResults;
        final Map<Integer, TimingResult> consumerResults;
    }

}
