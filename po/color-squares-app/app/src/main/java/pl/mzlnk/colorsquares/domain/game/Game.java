package pl.mzlnk.colorsquares.domain.game;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplate;
import pl.mzlnk.colorsquares.domain.utils.ColorUtil;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class Game {

    private List<GamePlayer> players = new ArrayList<>();
    private GameMap map;
    private GameStatus status = GameStatus.CREATED;
    private List<GameObserver> observers = new ArrayList<>();
    private Timer timer = new Timer();

    public static Game fromMapTemplate(MapTemplate mapTemplate) {
        Game game = new Game();
        game.map = GameMap.fromMapTemplate(mapTemplate);

        return game;
    }

    public GameMap getGameMap() {
        return map;
    }

    public List<GamePlayer> getRegisteredPlayers() {
        return players;
    }

    public GameResult getGameResult() {
        if (status != GameStatus.FINISHED) {
            return null;
        }

        return GameResult.fromGame(this);
    }

    public void registerGamePlayer(GamePlayer player) {
        players.add(player);
        setStatus(GameStatus.READY_TO_RUN);

        addPlayerToMap(player);
    }

    public void startGame() {
        setStatus(GameStatus.RUNNING);

        timer.scheduleAtFixedRate(new GameTask(), 3000L, 50L);
    }

    public void cancelGame() {
        if (timer != null) {
            timer.cancel();
            timer.purge();
        }
    }

    public void registerObserver(GameObserver observer) {
        observers.add(observer);
    }

    public void unregisterObserver(GameObserver observer) {
        observers.remove(observer);
    }

    private void addPlayerToMap(GamePlayer player) {
        if (player.isAi()) {
            player.setColor(ColorUtil.randomColor());
            player.setStartPosition(map.getRandomNotOccupiedPosition());
        }
        map.addPlayerToMap(player);
    }

    private void setStatus(GameStatus status) {
        this.status = status;
        onGameStatusChanged();
    }

    private void onGameStatusChanged() {
        for (GameObserver observer : observers) {
            observer.onGameStatusChanged(status);
        }
    }

    private void onMapUpdated() {
        for (GameObserver observer : observers) {
            observer.onMapUpdated(map.getMap());
        }
    }

    private void onGameFinished() {
        for (GameObserver observer : observers) {
            observer.onGameFinished(this);
        }
    }

    private class GameTask extends TimerTask {

        @Override
        public void run() {
            map.expandAreas();
            onMapUpdated();

            if (map.isAllPositionsOccupied()) {
                setStatus(GameStatus.FINISHED);
                onGameFinished();

                timer.cancel();
                timer.purge();
            }
        }

    }

}
