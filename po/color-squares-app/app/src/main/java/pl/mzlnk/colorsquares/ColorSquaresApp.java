package pl.mzlnk.colorsquares;

import android.app.Application;

import pl.mzlnk.colorsquares.domain.game.Game;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplateRepository;

public class ColorSquaresApp extends Application {

    public static ColorSquaresApp app;

    public MapTemplateRepository mapTemplateRepository;
    public Game currentGame;

    @Override
    public void onCreate() {
        super.onCreate();

        app = this;

        mapTemplateRepository = MapTemplateRepository.init(this.getApplicationContext());
        currentGame = null;
    }

}
