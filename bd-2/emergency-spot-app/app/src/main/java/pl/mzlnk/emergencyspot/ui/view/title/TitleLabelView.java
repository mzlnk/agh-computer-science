package pl.mzlnk.emergencyspot.ui.view.title;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.TextView;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.view.BaseRelativeLayoutView;

public class TitleLabelView extends BaseRelativeLayoutView {

    private TextView label;

    private int labelResId;

    public TitleLabelView(Context context) {
        super(context);
    }

    public TitleLabelView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    @Override
    protected int xmlLayoutResId() {
        return R.layout.v_title_label;
    }

    @Override
    protected void readAttributes(AttributeSet attributeSet) {
        TypedArray arr = this.getContext().obtainStyledAttributes(attributeSet, R.styleable.TitleLabelView, 0, 0);

        this.labelResId = arr.getResourceId(R.styleable.TitleLabelView_label, R.string.empty);

        arr.recycle();
    }

    @Override
    protected void loadViewsFromXml() {
        this.label = findViewById(R.id.v_title_label_text_label);
    }

    @Override
    protected void prepareViews() {
        this.label.setText(labelResId);
    }

    public void setLabel(String label) {
        this.label.setText(label);
    }

}
