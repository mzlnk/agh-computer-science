package pl.mzlnk.colorsquares.ui.fragment.gamesetup;

import android.os.Bundle;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplate;
import pl.mzlnk.colorsquares.ui.fragment.BaseFragment;
import pl.mzlnk.colorsquares.ui.view.MapCanvas;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameSetupMapExampleFragment extends BaseFragment {

    private static final String MAP_TEMPLATE_ID_PARAM = "game.setup.map.example.mapTemplateParam";

    private RelativeLayout grid;
    private TextView mapTemplateName;

    private MapTemplate mapTemplate;

    public GameSetupMapExampleFragment() {
        // Required empty public constructor
    }

    public static GameSetupMapExampleFragment newInstance(String mapTemplateId) {
        GameSetupMapExampleFragment fragment = new GameSetupMapExampleFragment();

        Bundle args = new Bundle();
        args.putString(MAP_TEMPLATE_ID_PARAM, mapTemplateId);
        fragment.setArguments(args);

        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstance) {
        super.onCreate(savedInstance);

        if (this.getArguments() != null) {
            String mapTemplateId = this.getArguments().getString(MAP_TEMPLATE_ID_PARAM);
            this.mapTemplate = app.mapTemplateRepository.findById(mapTemplateId).orElseThrow(MapTemplateNotFoundException::new);
        }
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_game_setup_map_example;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        grid = rootView.findViewById(R.id.f_game_setup_map_example_layout_grid);
        mapTemplateName = rootView.findViewById(R.id.f_game_setup_map_example_text_map_name);
    }

    @Override
    protected void prepareViews() {
        mapTemplateName.setText(mapTemplate.getName());
        grid.addView(new MapCanvas(this.getContext(), mapTemplate));
    }

    private static final class MapTemplateNotFoundException extends RuntimeException {

        public MapTemplateNotFoundException() {
            super();
        }

        public MapTemplateNotFoundException(String message) {
            super(message);
        }

    }

}
