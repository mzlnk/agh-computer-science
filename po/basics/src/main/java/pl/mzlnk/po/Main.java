package pl.mzlnk.po;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Main {

    public static void main(String[] args) {
        int[] aList = {2, 3, 9, 2, 5, 1, 3, 7, 10};
        int[] bList = {2, 1, 3, 4, 3, 10, 6, 6, 1, 7, 10, 10, 10};

        Map<Integer, Integer> occurs = new HashMap<>();
        for (int b : bList) {
            occurs.putIfAbsent(b, 0);
            occurs.computeIfPresent(b, (k, v) -> v = v + 1);
        }

        Map<Integer, Boolean> primeOccurs = occurs.keySet()
                .stream()
                .collect(Collectors.toMap(k -> k, k -> isPrime(occurs.get(k))));


        int[] c = Arrays.stream(aList)
                .filter(a -> !primeOccurs.getOrDefault(a, false))
                .toArray();

        Arrays.stream(c)
                .forEach(System.out::println);
    }

    private static boolean isPrime(int v) {
        if(v <= 1) return false;
        if(v <= 3) return true;
        if(v % 2 == 0 || v % 3 == 0) return false;

        for(int i = 5; i * i < v; i += 6) {
            if(v % i == 0 || v % (i + 1) == 0) return false;
        }
        return true;
    }


}
