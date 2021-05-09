package pl.mzlnk.emergencyspot.ui.animation;

import android.animation.AnimatorSet;
import android.animation.ObjectAnimator;
import android.view.View;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class FadeInAnimation extends BaseAnimation {

    private View view;
    private long initDelay;
    private long duration;

    public FadeInAnimation(View view) {
        this(view, 0, 200);
    }

    @Override
    protected AnimatorSet prepareAnimation() {
        ObjectAnimator initAnimator = ObjectAnimator.ofFloat(this.view, "alpha", 0F, 0F);
        initAnimator.setDuration(0);
        initAnimator.setStartDelay(0);

        ObjectAnimator animator = ObjectAnimator.ofFloat(this.view, "alpha", 0F, 1F);
        animator.setDuration(duration);
        animator.setStartDelay(initDelay);

        AnimatorSet set = new AnimatorSet();
        set.playTogether(initAnimator, animator);

        return set;
    }

}
