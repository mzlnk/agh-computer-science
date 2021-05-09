package pl.mzlnk.colorsquares.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.view.View;

import java.util.HashMap;
import java.util.Map;

import pl.mzlnk.colorsquares.domain.game.GameMap;
import pl.mzlnk.colorsquares.domain.game.GameObserver;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplate;

public class MapCanvas extends View implements GameObserver {

    private static final int WALL_COLOR_ID = -1;
    private static final int NONE_AREA_COLOR = Color.TRANSPARENT;
    private static final int WALL_COLOR = Color.WHITE;

    private int[][] map;
    private int size;

    private Map<Integer, Integer> colors = new HashMap<>();
    private Paint paint = new Paint();

    private MapCanvas(Context context) {
        super(context);

        this.colors.put(WALL_COLOR_ID, WALL_COLOR);
    }

    public MapCanvas(Context context, MapTemplate template) {
        this(context);

        this.map = template.getMap();
        this.size = template.getSize();

        this.setZ(0);
    }

    public MapCanvas(Context context, GameMap gameMap) {
        this(context);

        this.map = gameMap.getMap();
        this.size = gameMap.getSize();
        this.colors.putAll(gameMap.getColors());
    }

    @Override
    public void onDraw(Canvas canvas) {
        float unitSize = 1.0f * this.getWidth() / size;

        paint.setColor(Color.GRAY);
        paint.setStyle(Paint.Style.FILL);

        for (int x = 0; x < size; x++) {
            for (int y = 0; y < size; y++) {
                paint.setColor(colors.getOrDefault(map[x][y], NONE_AREA_COLOR));
                canvas.drawRect(
                        x * unitSize,
                        y * unitSize,
                        (x + 1) * unitSize,
                        (y + 1) * unitSize,
                        paint
                );
            }
        }
    }

    @Override
    public void onMapUpdated(int[][] map) {
        this.map = map;
        this.invalidate();
    }

}
