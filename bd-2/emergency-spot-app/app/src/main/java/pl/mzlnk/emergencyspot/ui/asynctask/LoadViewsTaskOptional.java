package pl.mzlnk.emergencyspot.ui.asynctask;

import android.app.Activity;
import android.widget.LinearLayout;

import androidx.fragment.app.FragmentManager;

import java.lang.ref.WeakReference;
import java.util.function.Supplier;

public class LoadViewsTaskOptional {

    public static LoadViewsTaskOptional of(Activity activity, LinearLayout container, FragmentManager fragmentManager) {
        return new LoadViewsTaskOptional(activity, container, fragmentManager, false);
    }

    private WeakReference<Activity> activityWeakRef;
    private WeakReference<LinearLayout> containerWeakRef;
    private WeakReference<FragmentManager> fragmentManagerWeakRef;

    private boolean cancelled;

    private LoadViewsTaskOptional(Activity activity, LinearLayout container, FragmentManager fragmentManager, boolean cancelled) {
        this.activityWeakRef = new WeakReference<>(activity);
        this.containerWeakRef = new WeakReference<>(container);
        this.fragmentManagerWeakRef = new WeakReference<>(fragmentManager);
        this.cancelled = cancelled;
    }

    public LoadViewsTaskOptional checkIfCancelled(Supplier<Boolean> condition) {
        this.cancelled = condition.get();

        return this;
    }

    public LoadViewsTaskOptional checkIfActivityPresent() {
        return checkIfCancelled(() -> activityWeakRef.get() == null);
    }

    public LoadViewsTaskOptional checkIfContainerPresent() {
        return checkIfCancelled(() -> containerWeakRef.get() == null);
    }

    public LoadViewsTaskOptional checkIfFragmentManagerPresent() {
        return checkIfCancelled(() -> fragmentManagerWeakRef.get() == null);
    }

    public void perform(TriConsumer<Activity, LinearLayout, FragmentManager> action) {
        if(!cancelled) {
            action.apply(activityWeakRef.get(), containerWeakRef.get(), fragmentManagerWeakRef.get());
        }
    }

    public void perform(BiConsumer<Activity, LinearLayout> action) {
        if(!cancelled) {
            action.apply(activityWeakRef.get(), containerWeakRef.get());
        }
    }

    public void perform(Consumer<Activity> action) {
        if(!cancelled) {
            action.apply(activityWeakRef.get());
        }
    }

    public interface Consumer<K> {

        void apply(K k);

    }

    public interface BiConsumer<K, V> {

        void apply(K k, V v);

    }

    public interface TriConsumer<K, V, E> {

        void apply(K k, V v, E e);

    }

}
