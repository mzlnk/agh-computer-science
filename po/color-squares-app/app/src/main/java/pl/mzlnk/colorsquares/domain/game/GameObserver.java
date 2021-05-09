package pl.mzlnk.colorsquares.domain.game;

public interface GameObserver {

    default void onGameStatusChanged(GameStatus newGameStatus) {

    }

    default void onMapUpdated(int[][] map) {

    }

    default void onGameFinished(Game game) {

    }

}
