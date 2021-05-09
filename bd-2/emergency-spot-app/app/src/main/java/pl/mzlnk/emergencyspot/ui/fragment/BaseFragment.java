package pl.mzlnk.emergencyspot.ui.fragment;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.fragment.app.Fragment;

public abstract class BaseFragment extends Fragment {

    @Override
    public final void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if(this.getArguments() != null) {
            readArguments(this.getArguments());
        }
    }

    @Override
    public final View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = inflater.inflate(this.getInflatedLayoutResId(), container, false);
        prepareData();
        prepareLayout(rootView);
        registerObservers();
        return rootView;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        unregisterObservers();
    }

    protected abstract int getInflatedLayoutResId();

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
        prepareAnimations();
        prepareViews();
        prepareListeners();
    }

}
