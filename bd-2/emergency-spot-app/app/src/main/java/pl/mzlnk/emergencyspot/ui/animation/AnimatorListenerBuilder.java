package pl.mzlnk.emergencyspot.ui.animation;

import android.animation.Animator;

import java.util.function.Consumer;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class AnimatorListenerBuilder {

    private Consumer<Animator> onAnimationStartConsumer;
    private Consumer<Animator> onAnimationEndConsumer;
    private Consumer<Animator> onAnimationCancelConsumer;
    private Consumer<Animator> onAnimationRepeatConsumer;

    public AnimatorListenerBuilder withAnimationStartAction(Consumer<Animator> action) {
        onAnimationStartConsumer = action;
        return this;
    }

    public AnimatorListenerBuilder withAnimationEndAction(Consumer<Animator> action) {
        onAnimationEndConsumer = action;
        return this;
    }

    public AnimatorListenerBuilder withAnimationCancelAction(Consumer<Animator> action) {
        onAnimationCancelConsumer = action;
        return this;
    }

    public AnimatorListenerBuilder withAnimationRepeatAction(Consumer<Animator> action) {
        onAnimationRepeatConsumer = action;
        return this;
    }

    public Animator.AnimatorListener build() {
        return new Animator.AnimatorListener() {
            @Override
            public void onAnimationStart(Animator animation) {
                if(onAnimationStartConsumer != null) {
                    onAnimationStartConsumer.accept(animation);
                }
            }

            @Override
            public void onAnimationEnd(Animator animation) {
                if(onAnimationEndConsumer != null) {
                    onAnimationEndConsumer.accept(animation);
                }
            }

            @Override
            public void onAnimationCancel(Animator animation) {
                if(onAnimationCancelConsumer != null) {
                    onAnimationCancelConsumer.accept(animation);
                }
            }

            @Override
            public void onAnimationRepeat(Animator animation) {
                if(onAnimationRepeatConsumer != null) {
                    onAnimationRepeatConsumer.accept(animation);
                }
            }
        };
    }

}
