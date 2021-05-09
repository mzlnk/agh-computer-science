package pl.mzlnk.emergencyspot.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;

public abstract class BaseRelativeLayoutView extends RelativeLayout {

    public BaseRelativeLayoutView(Context context) {
        this(context, null);
    }

    public BaseRelativeLayoutView(Context context, AttributeSet attributeSet) {
        this(context, attributeSet, 0);
    }

    public BaseRelativeLayoutView(Context context, AttributeSet attributeSet, int defStyleAttr) {
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
