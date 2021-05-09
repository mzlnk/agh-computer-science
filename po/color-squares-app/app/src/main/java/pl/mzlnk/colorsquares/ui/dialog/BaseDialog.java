package pl.mzlnk.colorsquares.ui.dialog;

import android.app.Dialog;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.view.Window;

public abstract class BaseDialog extends Dialog implements View.OnClickListener {

    public BaseDialog(Context context) {
        super(context);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        setContentView(this.getContentViewResId());
        prepareLayout();
    }

    protected abstract int getContentViewResId();

    protected void prepareData() {

    }

    protected void loadViewsFromXml() {

    }

    protected void prepareAnimations() {

    }

    protected void prepareViews() {

    }

    private void prepareLayout() {
        prepareData();
        loadViewsFromXml();
        prepareAnimations();
        prepareViews();
    }

    @Override
    public void onClick(View view) {

    }

}
