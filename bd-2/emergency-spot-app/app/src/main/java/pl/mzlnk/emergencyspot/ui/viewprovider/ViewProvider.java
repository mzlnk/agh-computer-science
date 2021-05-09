package pl.mzlnk.emergencyspot.ui.viewprovider;

import android.content.Context;
import android.view.View;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;

import java.util.List;
import java.util.Map;

public interface ViewProvider<T, K, V extends View> {

    List<Map.Entry<K, V>> provide(List<T> data, Context context, @Nullable FragmentManager fragmentManager);

}
