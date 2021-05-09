package pl.mzlnk.evolution.api.utils;

import lombok.AllArgsConstructor;

import static pl.mzlnk.evolution.api.utils.RandomUtil.r;

@AllArgsConstructor
public class Triple {

    public final int a;
    public final int b;
    public final int c;

    public static class TripleFactory {

        public static Triple fromPositiveNumbersWithSum(int sum, boolean nonZeroRequired) {
            if (nonZeroRequired) {
                sum -= 2;
            }

            if (sum < 0) throw new IllegalArgumentException("Cannot make triple from given sum");

            int a = r.nextInt(sum);
            int b = r.nextInt(sum - a);
            int c = sum - (a + b);

            if (nonZeroRequired) {
                a += 1;
                b += 1;
                c += 1;
            }

            return new Triple(a, b, c);
        }

    }

}
