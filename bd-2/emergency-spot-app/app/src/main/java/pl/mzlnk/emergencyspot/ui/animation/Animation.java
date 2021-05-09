package pl.mzlnk.emergencyspot.ui.animation;

import androidx.annotation.StringRes;

import pl.mzlnk.emergencyspot.R;

public interface Animation {

    @StringRes
    int TAG = R.string.animation_tag;

    void addOnAnimationEndAction(Runnable action);
    void start();
    void end();

}
