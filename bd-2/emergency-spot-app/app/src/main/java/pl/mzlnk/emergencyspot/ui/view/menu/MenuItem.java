package pl.mzlnk.emergencyspot.ui.view.menu;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.widget.ImageView;
import android.widget.TextView;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.view.BaseLinearLayoutView;

public class MenuItem extends BaseLinearLayoutView implements SelectableItem {

    private ImageView icon;
    private TextView label;

    private int iconResId;
    private int labelResId;

    private boolean selected = false;

    public MenuItem(Context context) {
        super(context);
    }

    public MenuItem(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    @Override
    protected int xmlLayoutResId() {
        return R.layout.v_menu_item;
    }

    @Override
    protected void readAttributes(AttributeSet attributeSet) {
        TypedArray arr = this.getContext().obtainStyledAttributes(attributeSet, R.styleable.MenuItem, 0, 0);

        this.iconResId = arr.getResourceId(R.styleable.MenuItem_icon, R.drawable.d_menu_item_hospital_stay);
        this.labelResId = arr.getResourceId(R.styleable.MenuItem_label, R.string.empty);

        arr.recycle();
    }

    @Override
    protected void loadViewsFromXml() {
        this.icon = findViewById(R.id.v_menu_item_img_icon);
        this.label = findViewById(R.id.v_menu_item_text_label);
    }

    @Override
    protected void prepareViews() {
        this.icon.setBackgroundResource(iconResId);
        this.label.setText(labelResId);
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        boolean result = super.onTouchEvent(event);

        if (event.getAction() == MotionEvent.ACTION_DOWN) {
            this.icon.setAlpha(1F);
            this.label.setAlpha(1F);
        }

        if (event.getAction() == MotionEvent.ACTION_UP) {
            this.icon.setAlpha(this.selected ? 1F : 0.5F);
            this.label.setAlpha(this.selected ? 1F : 0.5F);
        }

        return result;
    }

    @Override
    public void setItemSelected(boolean selected) {
        this.selected = selected;
        updateUi();
    }

    private void updateUi() {
        this.icon.setAlpha(this.selected ? 1 : 0.5F);
        this.label.setAlpha(this.selected ? 1 : 0.5F);
    }

}
