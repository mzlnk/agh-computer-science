package pl.mzlnk.colorsquares.ui.view;

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.widget.LinearLayout;

public class CustomLinearLayout extends LinearLayout {


    public CustomLinearLayout(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);

        prepareLayout();
    }

    private void prepareLayout() {
        inflate();
        readAttributes();
        loadViewsFromXml();
        prepareViews();
        prepareListeners();
    }

    protected void inflate() {

    }

    protected void readAttributes() {

    }

    protected void loadViewsFromXml() {

    }

    protected void prepareViews() {

    }

    protected void prepareListeners() {

    }

}
