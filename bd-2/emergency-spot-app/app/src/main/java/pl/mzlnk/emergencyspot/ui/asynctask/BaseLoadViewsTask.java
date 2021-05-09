package pl.mzlnk.emergencyspot.ui.asynctask;

import android.os.AsyncTask;
import android.os.SystemClock;
import android.view.View;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

import lombok.AccessLevel;
import lombok.Getter;
import pl.mzlnk.emergencyspot.ui.animation.Animation;
import pl.mzlnk.emergencyspot.ui.animation.FadeInAnimation;
import pl.mzlnk.emergencyspot.ui.viewprovider.ViewProvider;

public abstract class BaseLoadViewsTask<T, K, V extends View> extends AsyncTask<Void, Void, Void> implements LoadViewsTask<T> {

    @Getter(AccessLevel.PROTECTED)
    private LoadViewsTaskOptional resources;

    private BiConsumer<K, V> onLoadFinishedAction;
    private Map<K, V> items = new HashMap<>();

    private List<T> data;

    BaseLoadViewsTask(LoadViewsTaskReceiver<T, K, V> receiver) {
        this.resources = LoadViewsTaskOptional.of(
                receiver.getActivity(),
                receiver.getContainer(),
                receiver.getFragmentManager()
        );

        this.onLoadFinishedAction = receiver.onLoadFinishedAction();
    }

    protected abstract ViewProvider<T, K, V> viewProvider();

    @Override
    public final void cancelTask(boolean mayInterruptIfRunning) {
        super.cancel(mayInterruptIfRunning);
    }

    @Override
    public final boolean isTaskCancelled() {
        return super.isCancelled();
    }

    @Override
    public final void executeTask(List<T> data) {
        this.data = data;

        super.executeOnExecutor(THREAD_POOL_EXECUTOR);
    }

    @Override
    protected Void doInBackground(Void... voids) {
        resources
                .checkIfCancelled(super::isCancelled)
                .checkIfActivityPresent()
                .checkIfFragmentManagerPresent()
                .checkIfContainerPresent()
                .perform((a, c, fm) -> {
                    c.removeAllViewsInLayout();

                    viewProvider()
                            .provide(this.data, a, fm)
                            .forEach(entry -> {
                                items.put(entry.getKey(), entry.getValue());

                                resources
                                        .checkIfCancelled(super::isCancelled)
                                        .checkIfActivityPresent()
                                        .checkIfContainerPresent()
                                        .perform((a2, c2) -> a2.runOnUiThread(() -> {
                                            c2.addView(entry.getValue());

                                            Animation animation = new FadeInAnimation(entry.getValue());
                                            animation.start();
                                        }));

                                SystemClock.sleep(2);
                            });
                });


        return null;
    }

    @Override
    protected void onPostExecute(Void aVoid) {
        super.onPostExecute(aVoid);

        if(isCancelled() || this.items == null) {
            return;
        }

        this.items
                .entrySet()
                .forEach(entry -> onLoadFinishedAction.accept(entry.getKey(), entry.getValue()));

        cleanUp();
    }

    @Override
    protected void onCancelled(Void aVoid) {
        super.onCancelled(aVoid);
        cleanUp();
    }

    @Override
    protected void onCancelled() {
        super.onCancelled();
        cleanUp();
    }

    private void cleanUp() {
        if(this.items != null) {
            this.items.clear();
        }
        this.onLoadFinishedAction = null;
        this.items = null;
    }

}
