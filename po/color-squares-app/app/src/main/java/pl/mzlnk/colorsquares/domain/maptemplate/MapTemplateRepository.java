package pl.mzlnk.colorsquares.domain.maptemplate;

import android.content.Context;
import android.util.Log;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MapTemplateRepository {

    private Map<String, MapTemplate> templates = new HashMap<>();

    public static MapTemplateRepository init(Context context) {
        MapTemplateRepository repository = new MapTemplateRepository();

        try {
            for (String asset : context.getAssets().list("maps")) {
                MapTemplate template = MapTemplate.fromInputStream(context.getAssets().open("maps/" + asset));
                repository.templates.put(template.getId(), template);
            }
        } catch (IOException e) {
            e.printStackTrace();
            // todo: code here
        }

        Log.i("map-template", "Loaded " + repository.templates.size() + " map templates");

        return repository;
    }

    public Optional<MapTemplate> findById(String id) {
        return Optional.ofNullable(templates.get(id));
    }

    public List<MapTemplate> findAll() {
        return new ArrayList<>(templates.values());
    }

}
