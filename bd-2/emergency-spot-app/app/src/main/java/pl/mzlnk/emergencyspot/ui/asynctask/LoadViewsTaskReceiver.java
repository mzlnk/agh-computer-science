package pl.mzlnk.emergencyspot.ui.asynctask;

import android.app.Activity;
import android.widget.LinearLayout;

import androidx.fragment.app.FragmentManager;

import java.util.function.BiConsumer;

public interface LoadViewsTaskReceiver<T, K, V> {

    LoadViewsTask<T> loadViewsTask();

    Activity getActivity();

    LinearLayout getContainer();

    FragmentManager getFragmentManager();

    default BiConsumer<K, V> onLoadFinishedAction() {
        return (k, v) -> {
        };
    }

}
