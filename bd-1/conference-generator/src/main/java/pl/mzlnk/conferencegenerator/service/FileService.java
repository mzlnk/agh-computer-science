package pl.mzlnk.conferencegenerator.service;

import lombok.Getter;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public interface FileService {

    Optional<File> findFile(Directory directory, String filename);
    List<File> listFiles(Directory directory);
    void createOrUpdateFile(Directory directory, String filename, String str);
    void removeFile(Directory directory, String filename);

    public enum Directory {

        RESULT("result"),
        PROPERTIES("properties"),
        DATA("data");

        @Getter
        private String directory;

        private Directory(String directory) {
            this.directory = directory;
        }

        public static List<Directory> listDirectories() {
            return Arrays.asList(Directory.values());
        }

    }

}
