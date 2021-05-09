package pl.mzlnk.conferencegenerator.repository.data;

import com.google.gson.Gson;
import pl.mzlnk.conferencegenerator.model.data.ConferenceDetails;
import pl.mzlnk.conferencegenerator.model.data.DataType;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static pl.mzlnk.conferencegenerator.utils.RandomUtil.r;

class ConferenceDetailsRepository extends BaseDataRepository<ConferenceDetails> {

    private static final String NAMES_FILE = "conference-details.json";

    private List<ConferenceDetails> details;

    private int detailsSize;

    ConferenceDetailsRepository(FileService fileService) {
        super(DataType.CONFERENCE_DETAILS, fileService);
    }

    @Override
    protected ConferenceDetails randomEntry() {
        ConferenceDetails conference = details.get(r.nextInt(this.detailsSize));
        conference.setStudentDiscount(r.nextDouble() * 100);
        return conference;
    }

    @Override
    protected void loadData(FileService fileService) {
        Gson gson = new Gson();

        fileService.findFile(FileService.Directory.DATA, NAMES_FILE)
                .ifPresent(file -> {
                    try (FileReader fr = new FileReader(file)) {
                        details = Arrays.asList(gson.fromJson(fr, ConferenceDetails[].class));
                        detailsSize = details.size();
                        System.out.println("Loaded " + this.detailsSize + " conference details");
                    } catch (IOException e) {
                        e.printStackTrace(); // todo: add log support here
                    }
                });
    }

}
