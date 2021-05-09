package pl.mzlnk.conferencegenerator.repository.data;

import com.google.gson.Gson;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.model.data.Person;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class PersonRepository extends BaseDataRepository<Person> {

    private static final String FIRST_NAMES_FILE = "person-firstnames.json";
    private static final String LAST_NAMES_FILE = "person-lastnames.json";

    private List<String> firstNames;
    private List<String> lastNames;

    private int firstNamesSize;
    private int lastNamesSize;

    PersonRepository(FileService fileService) {
        super(DataType.PERSON, fileService);
    }

    @Override
    protected Person randomEntry() {
        String firstName = firstNames.get(r.nextInt(firstNamesSize));
        String lastName = lastNames.get(r.nextInt(lastNamesSize));

        return new Person(firstName, lastName);
    }

    @Override
    protected void loadData(FileService fileService) {
        Gson gson = new Gson();

        fileService.findFile(FileService.Directory.DATA, FIRST_NAMES_FILE)
                .ifPresent(file -> {
                    try (FileReader fr = new FileReader(file)) {
                        this.firstNames = Arrays.asList(gson.fromJson(fr, String[].class));
                        this.firstNamesSize = this.firstNames.size();
                    } catch (IOException e) {
                        e.printStackTrace(); // todo: add log support here
                    }
                });

        fileService.findFile(FileService.Directory.DATA, LAST_NAMES_FILE)
                .ifPresent(file -> {
                    try (FileReader fr = new FileReader(file)) {
                        this.lastNames = Arrays.asList(gson.fromJson(fr, String[].class));
                        this.lastNamesSize = this.lastNames.size();
                    } catch (IOException e) {
                        e.printStackTrace(); // todo: add log support here
                    }
                });
    }

}
