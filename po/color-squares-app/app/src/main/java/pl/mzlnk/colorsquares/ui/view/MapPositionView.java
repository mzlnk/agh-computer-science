package pl.mzlnk.colorsquares.ui.view;

import android.content.Context;
import android.graphics.Point;
import android.view.View;
import android.widget.RelativeLayout;

public class MapPositionView extends View {

    private int x;
    private int y;

    private int size;

    public MapPositionView(Context context, int size, Point position) {
        super(context);

        this.size = size;
        this.x = position.x;
        this.y = position.y;

        prepareView();
    }

    public void setColor(int color) {
        setBackgroundColor(color);
    }

    private void prepareView() {
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(size, size);
        params.setMargins(x * size, y * size, 0, 0);

        setLayoutParams(params);
    }

}
