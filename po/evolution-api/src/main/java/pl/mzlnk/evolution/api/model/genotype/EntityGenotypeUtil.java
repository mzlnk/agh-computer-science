package pl.mzlnk.evolution.api.model.genotype;

import lombok.AllArgsConstructor;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static pl.mzlnk.evolution.api.model.genotype.EntityGenotype.MAX_GENE_VALUE;

/**
 * Created by Marcin Zielonka on 2019.12.14
 */

@AllArgsConstructor
public class EntityGenotypeUtil {

    private EntityGenotype genotype;

    public Map<Integer, Integer> getGenesCountMap() {
        Map<Integer, Integer> map = new HashMap<>();

        IntStream.range(0, MAX_GENE_VALUE + 1).forEach(i -> map.put(i, 0));
        genotype.genesStream().forEach(i -> map.compute(i, (k, v) -> v += 1));

        return map;
    }

    public Integer[] getGenesCountList() {
        return getGenesCountMap()
                .entrySet()
                .stream()
                .sorted(Comparator.comparingInt(Map.Entry::getKey))
                .map(Map.Entry::getValue)
                .toArray(Integer[]::new);
    }

    public Set<Integer> getMissingGenes() {
        Map<Integer, Integer> map = this.getGenesCountMap();

        return map
                .keySet()
                .stream()
                .filter(gene -> map.get(gene) == 0)
                .collect(Collectors.toSet());
    }

    public boolean areAllGenesExist() {
        return IntStream.range(0, MAX_GENE_VALUE + 1)
                .allMatch(genotype::geneExists);
    }

}
