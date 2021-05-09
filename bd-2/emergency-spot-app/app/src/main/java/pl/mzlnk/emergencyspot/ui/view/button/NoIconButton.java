package pl.mzlnk.emergencyspot.ui.view.button;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.MotionEvent;
import android.widget.TextView;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.view.BaseLinearLayoutView;

public class NoIconButton extends BaseLinearLayoutView {

    public static final int COLOR_LIME = Color.rgb(41, 171, 135);

    private TextView label;

    private int labelResId;
    private float labelSize;

    public NoIconButton(Context context) {
        super(context);
    }

    public NoIconButton(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    @Override
    protected int xmlLayoutResId() {
        return R.layout.v_no_icon_button;
    }

    @Override
    protected void readAttributes(AttributeSet attributeSet) {
        TypedArray arr = this.getContext().obtainStyledAttributes(attributeSet, R.styleable.NoIconButton, 0, 0);

        this.labelResId = arr.getResourceId(R.styleable.NoIconButton_label, R.string.empty);
        this.labelSize = arr.getDimensionPixelSize(R.styleable.NoIconButton_labelSize, 20);

        arr.recycle();
    }

    @Override
    protected void loadViewsFromXml() {
        this.label = findViewById(R.id.v_no_icon_button_text_label);
    }

    @Override
    protected void prepareViews() {
        this.label.setText(labelResId);
        this.label.setTextSize(TypedValue.COMPLEX_UNIT_PX, labelSize);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            super.setAlpha(0.5F);
        }

        if (event.getAction() == MotionEvent.ACTION_UP) {
            super.setAlpha(1.0F);
        }

        return super.onTouchEvent(event);
    }

    public void setLabel(String label) {
        this.label.setText(label);
    }

}