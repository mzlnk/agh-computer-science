package pl.mzlnk.emergencyspot.ui.dialog;

import android.content.DialogInterface;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.FrameLayout;

import androidx.fragment.app.DialogFragment;

import pl.mzlnk.emergencyspot.R;

public abstract class BaseDialog extends DialogFragment implements View.OnClickListener, DialogInterface.OnDismissListener {

    private Button close;

    protected abstract int xmlLayoutResId();

    @Override public void onStart() {
        super.onStart();

        Window window = getDialog().getWindow();
        WindowManager.LayoutParams windowParams = window.getAttributes();
        windowParams.dimAmount = 0.9F;
        windowParams.flags |= WindowManager.LayoutParams.FLAG_DIM_BEHIND;
        window.setAttributes(windowParams);
    }

    @Override
    public void onActivityCreated(Bundle args) {
        super.onActivityCreated(args);

        getDialog()
                .getWindow()
                .getAttributes()
                .windowAnimations = R.style.DialogAnimation;
    }

    @Override
    public final void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (this.getArguments() != null) {
            readArguments(this.getArguments());
        }

        this.setCancelable(this.isCancellable());
    }

    @Override
    public final View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        FrameLayout rootView = (FrameLayout) inflater.inflate(R.layout.d_dialog_layout, container, false);

        inflater.inflate(this.xmlLayoutResId(), rootView, true);

        getDialog().getWindow().setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));

        prepareData();
        prepareLayout(rootView);
        registerObservers();

        return rootView;
    }

    @Override
    public void onClick(View view) {

    }

    @Override
    public void onDismiss(DialogInterface dialogInterface) {

    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        unregisterObservers();
    }

    protected boolean isCancellable() {
        return false;
    }

    protected boolean isCloseButtonVisible() {
        return true;
    }

    protected void loadViewsFromXml(View rootView) {

    }

    protected void prepareAnimations() {

    }

    protected void prepareViews() {

    }

    protected void prepareListeners() {

    }

    protected void prepareData() {

    }

    protected void registerObservers() {

    }

    protected void unregisterObservers() {

    }

    protected void readArguments(Bundle arguments) {

    }

    protected void prepareLayout(View rootView) {
        loadViewsFromXml(rootView);
        prepareCloseButton(rootView);
        prepareAnimations();
        prepareViews();
        prepareListeners();
    }

    protected void onCloseButtonClick(View view) {
        dismissAllowingStateLoss();
    }

    private void prepareCloseButton(View rootView) {
        this.close = rootView.findViewById(R.id.d_dialog_layout_btn_close);

        if (this.close == null) {
            return;
        }

        this.close.setOnClickListener(this::onCloseButtonClick);

        this.close.setOnTouchListener((v, event) -> {
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                v.setAlpha(0.5F);
            }

            if (event.getAction() == MotionEvent.ACTION_UP) {
                v.setAlpha(1.0F);
            }

            return false;
        });

        if (!isCloseButtonVisible()) {
            this.close.setVisibility(View.GONE);
        }
    }

}
