package pl.mzlnk.emergencyspot.ui.asynctask;

import java.util.List;

public interface LoadViewsTask<T> {

    void cancelTask(boolean mayInterruptIfRunning);
    boolean isTaskCancelled();
    void executeTask(List<T> data);

}
