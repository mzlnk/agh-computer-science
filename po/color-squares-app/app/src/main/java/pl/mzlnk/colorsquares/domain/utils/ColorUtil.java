package pl.mzlnk.colorsquares.domain.utils;

import android.graphics.Color;

import static pl.mzlnk.colorsquares.domain.utils.RandomUtil.r;

public class ColorUtil {

    public static int randomColor() {
        return Color.rgb(r.nextInt(256), r.nextInt(256), r.nextInt(256));
    }

}
