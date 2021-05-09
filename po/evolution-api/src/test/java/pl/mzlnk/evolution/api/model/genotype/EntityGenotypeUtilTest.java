package pl.mzlnk.evolution.api.model.genotype;

import org.junit.jupiter.api.Test;

class EntityGenotypeUtilTest {

    @Test
    void getGenesCount() {
        EntityGenotype genotype = EntityGenotype.randomGenotype();

        EntityGenotypeUtil util = new EntityGenotypeUtil(genotype);
        System.out.println(util.getGenesCountMap().toString());


    }
}