package pl.mzlnk.colorsquares.domain.game;

import android.graphics.Point;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplate;
import pl.mzlnk.colorsquares.domain.utils.IntegerUtil;

import static pl.mzlnk.colorsquares.domain.utils.RandomUtil.r;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class GameMap {

    private static final int[] ADJACENT_DX = {-1, 0, 0, 1};
    private static final int[] ADJACENT_DY = {0, -1, 1, 0};

    private int size;
    private int[][] map;

    private int areasOccupied = 0;
    private int wallAreas;

    private Set<GamePlayer> players = new HashSet<>();
    private Map<Integer, Integer> colors = new HashMap<>();

    public static GameMap fromMapTemplate(MapTemplate mapTemplate) {
        GameMap gameMap = new GameMap();

        gameMap.size = mapTemplate.getSize();
        gameMap.map = mapTemplate.getMap();
        gameMap.wallAreas = mapTemplate.getWallAreas();

        return gameMap;
    }

    public int getSize() {
        return size;
    }

    public int[][] getMap() {
        return Arrays
                .stream(this.map)
                .map(int[]::clone)
                .toArray(int[][]::new);
    }

    public Map<Integer, Integer> getColors() {
        return colors;
    }

    public Point getRandomNotOccupiedPosition() {
        Point pos = null;
        do {
            pos = new Point(r.nextInt(size), r.nextInt(size));
        } while (isOccupied(pos));

        return pos;
    }

    public int getAllPlayersAreaCount() {
        int sum = 0;
        for (int x = 0; x < size; x++) {
            for (int y = 0; y < size; y++) {
                if (map[x][y] != -1) sum++;
            }
        }
        return sum;
    }

    public int getPlayerAreaCount(GamePlayer player) {
        int sum = 0;
        for (int x = 0; x < size; x++) {
            for (int y = 0; y < size; y++) {
                if (map[x][y] == player.getId()) sum++;
            }
        }
        return sum;
    }

    public boolean isOccupied(Point pos) {
        return map[pos.x][pos.y] != 0;
    }

    public boolean isAllPositionsOccupied() {
        return areasOccupied == size * size - wallAreas;
    }

    public void addPlayerToMap(GamePlayer player) {
        players.add(player);
        colors.put(player.getId(), player.getColor());

        map[player.getStartPosition().x][player.getStartPosition().y] = player.getId();
        areasOccupied++;
    }

    public void expandAreas() {
        List<GamePlayer> playerList = new ArrayList<>(players);
        Collections.shuffle(playerList);

        playerList.forEach(player -> {
            final Set<Point> coloredPositions = new HashSet<>();

            for (int x = 0; x < size; x++) {
                for (int y = 0; y < size; y++) {
                    if (map[x][y] != player.getId()) continue;

                    for (int d = 0; d < 4; d++) {
                        int nx = x + ADJACENT_DX[d];
                        int ny = y + ADJACENT_DY[d];

                        Point pos = new Point(x, y);
                        Point nPos = new Point(nx, ny);
                        if (isInBounds(nPos) && !isOccupied(nPos) && !coloredPositions.contains(pos) && r.nextBoolean()) {
                            map[nx][ny] = player.getId();
                            areasOccupied++;
                            coloredPositions.add(nPos);
                        }
                    }
                }
            }
        });
    }

    private boolean isInBounds(Point pos) {
        return IntegerUtil.between(pos.x, 0, size - 1, true)
                && IntegerUtil.between(pos.y, 0, size - 1, true);
    }

}
