package pl.mzlnk.emergencyspot.ui.animation;

import android.animation.Animator;
import android.animation.AnimatorSet;

public abstract class BaseAnimation implements Animation {

    private AnimatorSet set;
    private Runnable endAction;

    protected abstract AnimatorSet prepareAnimation();

    @Override
    public void addOnAnimationEndAction(Runnable action) {
        this.endAction = action;
    }

    @Override
    public void start() {
        this.set = prepareAnimation();

        set.addListener(
                new AnimatorListenerBuilder()
                        .withAnimationStartAction(this::onAnimationStart)
                        .withAnimationCancelAction(this::onAnimationCancel)
                        .withAnimationRepeatAction(this::onAnimationRepeat)
                        .withAnimationEndAction(animator -> {
                            this.onAnimationEnd(animator);
                            if(endAction != null) {
                                endAction.run();
                            }
                        })
                        .build()

        );

        set.start();
    }

    @Override
    public void end() {
        if(this.set != null) {
            this.set.end();
        }
    }

    protected void onAnimationStart(Animator animator) {

    }

    protected void onAnimationEnd(Animator animator) {

    }

    protected void onAnimationCancel(Animator animator) {

    }

    protected void onAnimationRepeat(Animator animator) {

    }

}
