package pl.mzlnk.evolution.api.model.genotype;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.Arrays;
import java.util.stream.IntStream;

import static pl.mzlnk.evolution.api.utils.RandomUtil.r;


@ToString
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EntityGenotype {

    public static final int GENES_AMOUNT = 32;
    public static final int MAX_GENE_VALUE = 7;

    public static EntityGenotype emptyGenotype() {
        return new EntityGenotype();
    }

    public static EntityGenotype randomGenotype() {
        EntityGenotype entityGenotype = new EntityGenotype();

        for (int i = 0, j = 0; j <= MAX_GENE_VALUE; i++, j++) {
            entityGenotype.genes[i] = i;
        }
        for (int i = MAX_GENE_VALUE + 1; i < GENES_AMOUNT; i++) {
            entityGenotype.genes[i] = r.nextInt(MAX_GENE_VALUE + 1);
        }

        entityGenotype.orderGenes();
        return entityGenotype;
    }

    private int[] genes = new int[32];

    public int geneAt(int pos) {
        if (pos < 0 || pos >= 32) {
            throw new IllegalArgumentException("Gene position should be between 0 and 31");
        }
        return genes[pos];
    }

    public int[] getGenes() {
        return Arrays.copyOf(genes, GENES_AMOUNT);
    }

    public boolean geneExists(int gene) {
        return IntStream.of(genes)
                .anyMatch(i -> i == gene);
    }

    public IntStream genesStream() {
        return IntStream.of(this.genes);
    }

    public void orderGenes() {
        this.genes = IntStream.of(this.genes)
                .sorted()
                .toArray();
    }

    void setGenes(int[] genes) {
        this.genes = genes.clone();
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(this.genes);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) return true;
        if (other == null) return false;
        if (this.getClass() != other.getClass()) return false;
        EntityGenotype otherGenotype = (EntityGenotype) other;
        return Arrays.equals(this.genes, otherGenotype.genes);
    }

}
