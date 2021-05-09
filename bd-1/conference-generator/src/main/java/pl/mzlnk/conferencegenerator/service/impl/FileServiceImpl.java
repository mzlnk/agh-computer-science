package pl.mzlnk.conferencegenerator.service.impl;

import org.apache.commons.io.FileUtils;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static pl.mzlnk.conferencegenerator.service.FileService.Directory.*;

public class FileServiceImpl implements FileService {

    public static FileService init() {
        FileServiceImpl fileService = new FileServiceImpl();

        fileService.prepareDirectories();
        fileService.copyBuiltInFiles();

        return fileService;
    }

    private final File workingDirectory;

    private FileServiceImpl() {
        this.workingDirectory = new File(System.getProperty("user.dir"));
    }

    @Override
    public Optional<File> findFile(Directory directory, String filename) {
        File file = new File(dir(directory), filename);
        return Optional.ofNullable(file.exists() ? file : null);
    }

    @Override
    public List<File> listFiles(Directory directory) {
        File[] files = dir(directory).listFiles();
        return files != null ? Arrays.asList(files) : new ArrayList<>();
    }

    @Override
    public void createOrUpdateFile(Directory directory, String filename, String content) {
        File file = new File(dir(directory), filename);

        try {
            if (!file.exists()) {
                file.createNewFile();
            }

            BufferedWriter writer = new BufferedWriter(new FileWriter(file));
            writer.write(content);
            writer.flush();
            writer.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void removeFile(Directory directory, String filename) {
        this.findFile(directory, filename).ifPresent(file -> {
            file.delete();
        });
    }

    private File dir(Directory child) {
        return new File(workingDirectory, child.getDirectory());
    }

    private void prepareDirectories() {
        System.out.println("working dir: " + workingDirectory);
        Directory.listDirectories().stream().forEach(dir -> {
            File directory = dir(dir);
            System.out.println(directory.getAbsolutePath());
            if (!directory.exists()) {
                directory.mkdirs();
            }
        });
    }

    private void copyBuiltInFiles() {
        File appReadme = new File(workingDirectory, "README.txt");
        File configReadme = new File(dir(PROPERTIES), "README.txt");
        File dataReadme = new File(dir(DATA), "README.txt");
        File outReadme = new File(dir(RESULT), "README.txt");
        File databaseProperties = new File(dir(PROPERTIES), "database.properties");

        ClassLoader classLoader = this.getClass().getClassLoader();

        try {
            if (!appReadme.exists()) {
                FileUtils.copyFile(new File(classLoader.getResource("readme/app-readme.txt").getFile()), appReadme);
            }
            if (!configReadme.exists()) {
                FileUtils.copyFile(new File(classLoader.getResource("readme/config-readme.txt").getFile()), configReadme);
            }
            if(!dataReadme.exists()) {
                FileUtils.copyFile(new File(classLoader.getResource("readme/data-readme.txt").getFile()), dataReadme);
            }
            if(!outReadme.exists()) {
                FileUtils.copyFile(new File(classLoader.getResource("readme/result-readme.txt").getFile()), outReadme);
            }
            if(!databaseProperties.exists()) {
                FileUtils.copyFile(new File(classLoader.getResource("properties/database.properties").getFile()), databaseProperties);
            }
        } catch (IOException e) {
            e.printStackTrace();
            // todo: add log support here
        }
    }

}
