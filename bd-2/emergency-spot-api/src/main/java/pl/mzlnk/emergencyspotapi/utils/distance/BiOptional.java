package pl.mzlnk.emergencyspotapi.utils.distance;

import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Optional equivalent which supports two objects instead of one
 * @param <T> type of first object
 * @param <U> type of second object
 */
public class BiOptional<T, U> {

    /**
     * Create BiOptional instance from two objects
     * @param first first object
     * @param second second object
     * @param <T> type of first object
     * @param <U> type of second object
     * @return BiOptional instance with wrapped both objects
     */
    public static <T, U> BiOptional<T, U> of(Optional<T> first, Optional<U> second) {
        BiOptional<T, U> biOptional = new BiOptional<>();

        biOptional.first = first;
        biOptional.second = second;

        return biOptional;
    }

    private Optional<T> first;
    private Optional<U> second;

    /**
     * Perform proper action based on presence of both objects associated with BiOptional
     * @param action action to be performed if both objects are present
     * @param emptyAction action to be performed if at least one of the objects is null
     */
    public void ifPresentOrElse(BiConsumer<T, U> action, Runnable emptyAction) {
        this.first.ifPresentOrElse(first -> {
                    this.second.ifPresentOrElse(second -> {
                                action.accept(first, second);
                            },
                            emptyAction::run
                    );
                },
                emptyAction::run
        );
    }

    /**
     * Map BiOptional to Optional
     * @param mapper function which converts BiOptional to Optional
     * @param <R> type of result
     * @return Optional with given type and wrapped object or Optional.empty() if at least one of the objects in BiOptional is null
     */
    public <R> Optional<R> map(BiFunction<T, U, R> mapper) {
        if(this.first.isEmpty() || this.second.isEmpty()) {
            return Optional.empty();
        }

        return Optional.ofNullable(mapper.apply(first.get(), second.get()));
    }

}
