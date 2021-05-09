package pl.mzlnk.colorsquares.domain.maptemplate;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.stream.IntStream;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MapTemplate {

    private static final char WALL_AREA_CODE = '#';
    private static final char NONE_AREA_CODE = '+';

    private String id;
    private String name;

    private int size;
    private int wallAreas;

    private int[][] map;

    public static MapTemplate fromInputStream(InputStream inputStream) {
        MapTemplate template = new MapTemplate();

        try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
            template.id = br.readLine();
            template.name = br.readLine();
            template.size = Integer.parseInt(br.readLine());

            template.map = transpose(
                    br.lines()
                            .map(String::toCharArray)
                            .map(arr -> {
                                return IntStream
                                        .range(0, arr.length)
                                        .mapToObj(i -> arr[i])
                                        .map(c -> {
                                            if (c == WALL_AREA_CODE) {
                                                template.wallAreas++;
                                                return -1;
                                            }
                                            return 0;
                                        })
                                        .mapToInt(i -> i)
                                        .toArray();
                            })
                            .toArray(int[][]::new)
            );

        } catch (IOException e) {
            return null;
        }

        return template;
    }

    private static int[][] transpose(int[][] matrix) {
        int[][] result = new int[matrix.length][matrix.length];

        for (int i = 0; i < result.length; i++) {
            for (int j = 0; j < result.length; j++) {
                result[i][j] = matrix[j][i];
            }
        }

        return result;
    }

    public int[][] getMap() {
        return Arrays
                .stream(this.map)
                .map(int[]::clone)
                .toArray(int[][]::new);
    }

}
