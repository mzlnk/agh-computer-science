package pl.mzlnk.colorsquares.ui.fragment;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.fragment.app.Fragment;

public abstract class BaseFragment extends Fragment {

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = inflater.inflate(this.getInflatedLayoutResId(), container, false);
        prepareData();
        prepareLayout(rootView);
        return rootView;
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

    private void prepareLayout(View rootView) {
        loadViewsFromXml(rootView);
        prepareAnimations();
        prepareViews();
        prepareListeners();
    }

}
