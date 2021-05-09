package pl.mzlnk.conferencegenerator.repository.data;

import com.google.gson.Gson;
import pl.mzlnk.conferencegenerator.model.data.CompanyName;
import pl.mzlnk.conferencegenerator.model.data.ConferenceDetails;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class CompanyNamesRepository extends BaseDataRepository<CompanyName> {

    private static final String COMPANY_NAMES_FILE = "company-names.json";

    private List<String> companyNames;

    private int companyNamesSize;

    CompanyNamesRepository(FileService fileService) {
        super(DataType.COMPANY_NAME, fileService);
    }

    @Override
    public CompanyName randomEntry() {
        return null;
    }

    @Override
    public List<CompanyName> findAll() {
        return companyNames
                .stream()
                .map(CompanyName::new)
                .collect(Collectors.toList());
    }

    @Override
    protected void loadData(FileService fileService) {
        Gson gson = new Gson();

        fileService.findFile(FileService.Directory.DATA, COMPANY_NAMES_FILE)
                .ifPresent(file -> {
                    try (FileReader fr = new FileReader(file)) {
                        this.companyNames = Arrays.asList(gson.fromJson(fr, String[].class));
                        this.companyNamesSize = this.companyNames.size();
                        System.out.println("Loaded: " + companyNamesSize + " company names");
                    } catch (IOException e) {
                        e.printStackTrace(); // todo: add log support here
                    }
                });
    }

}
