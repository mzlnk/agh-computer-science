package pl.mzlnk.colorsquares.domain.game;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GameResult {

    @Getter
    private List<GamePlayerResult> results = new ArrayList<>();

    static GameResult fromGame(Game game) {
        GameResult gameResult = new GameResult();

        final int all = game.getGameMap().getAllPlayersAreaCount();

        gameResult.results = game.getRegisteredPlayers()
                .stream()
                .map(player -> {
                    int playerAreas = game.getGameMap().getPlayerAreaCount(player);
                    double percentage = Math.round(10000D * playerAreas / all) / 100D;

                    return new GamePlayerResult(player, percentage);
                })
                .sorted((r1, r2) -> Double.compare(r2.getArea(), r1.getArea()))
                .collect(Collectors.toList());

        return gameResult;
    }

    @Getter
    @AllArgsConstructor
    public static final class GamePlayerResult {

        private GamePlayer player;
        private double area;

    }

}
