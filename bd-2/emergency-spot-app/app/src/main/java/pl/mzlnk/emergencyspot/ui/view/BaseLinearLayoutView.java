package pl.mzlnk.emergencyspot.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.LinearLayout;

public abstract class BaseLinearLayoutView extends LinearLayout {

    public BaseLinearLayoutView(Context context) {
        this(context, null);
    }

    public BaseLinearLayoutView(Context context, AttributeSet attributeSet) {
        this(context, attributeSet, 0);
    }

    public BaseLinearLayoutView(Context context, AttributeSet attributeSet, int defStyleAttr) {
        super(context, attributeSet, defStyleAttr);

        prepareLayout(attributeSet);
    }

    protected abstract int xmlLayoutResId();

    protected void readAttributes(AttributeSet attributeSet) {

    }

    protected void loadViewsFromXml() {

    }

    protected void prepareViews() {

    }

    private void prepareLayout(AttributeSet attributeSet) {
        inflate(this.getContext(), xmlLayoutResId(), this);

        if (attributeSet != null) {
            readAttributes(attributeSet);
        }

        loadViewsFromXml();
        prepareViews();
    }

}
