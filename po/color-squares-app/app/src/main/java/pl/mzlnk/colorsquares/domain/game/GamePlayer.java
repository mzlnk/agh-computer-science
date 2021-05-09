package pl.mzlnk.colorsquares.domain.game;

import android.graphics.Point;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class GamePlayer {

    @EqualsAndHashCode.Include
    private int id;
    private String name;

    private boolean ai;

    @Setter
    private int color;
    @Setter
    private Point startPosition;

    public GamePlayer(int id, boolean ai) {
        this.id = id;
        this.ai = ai;

        this.name = ai ? ("Player " + id) : "You";
    }

}
