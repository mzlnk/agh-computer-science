package pl.mzlnk.evolution.api.model.genotype;

import pl.mzlnk.evolution.api.utils.Triple;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static pl.mzlnk.evolution.api.utils.RandomUtil.r;

public class ParentBasedGenotypeCreator implements EntityGenotypeCreator {

    private EntityGenotype parent1;
    private EntityGenotype parent2;

    private EntityGenotype child;

    public ParentBasedGenotypeCreator(EntityGenotype parent1, EntityGenotype parent2) {
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.child = EntityGenotype.emptyGenotype();
    }

    @Override
    public EntityGenotype createGenotype() {
        copyGenotypePartsFromParents();
        fixChildGenotype();

        child.orderGenes();

        return child;
    }

    private void copyGenotypePartsFromParents() {
        var triple = Triple.TripleFactory.fromPositiveNumbersWithSum(32, true);
        int[] genes = child.getGenes();

        for (int i = 0; i < triple.a; i++) {
            genes[i] = parent1.geneAt(i);
        }
        for (int i = 0; i < triple.a + triple.b; i++) {
            genes[i] = parent2.geneAt(i);
        }
        for (int i = triple.a + triple.b; i < 32; i++) {
            genes[i] = parent1.geneAt(i);
        }

        child.setGenes(genes);
    }

    private void fixChildGenotype() {
        EntityGenotypeUtil util = new EntityGenotypeUtil(this.child);
        if (util.areAllGenesExist()) {
            return;
        }

        Map<Integer, Integer> genesCount = util.getGenesCountMap();
        Set<Integer> missingGenes = util.getMissingGenes();

        int[] genes = new int[32];

        missingGenes.forEach(missingGene -> {
            List<Integer> availableSwapGenes = genesCount.entrySet()
                    .stream()
                    .filter(e -> e.getValue() >= 2)
                    .map(Map.Entry::getKey)
                    .collect(Collectors.toList());

            int swapGene = availableSwapGenes.get(r.nextInt(availableSwapGenes.size()));
            genesCount.compute(missingGene, (k, v) -> v++);
            genesCount.compute(swapGene, (k, v) -> v--);
        });

        int i = 0;
        for (int gene : genesCount.keySet()) {
            for (int j = 0; j < genesCount.get(gene); j++) {
                genes[i++] = gene;
            }
        }

        this.child.setGenes(genes);
    }

}
