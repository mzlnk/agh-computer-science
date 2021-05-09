package pl.mzlnk.conferencegenerator.generator.sql.utils.comparator;

import pl.mzlnk.conferencegenerator.generator.sql.utils.parser.SqlEntityRepositoryParser;
import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepository;

import java.util.Comparator;

public class EntityRepositoryDependencyComparator implements Comparator<EntityRepository<? extends Entity>> {

    @Override
    public int compare(EntityRepository<? extends Entity> r1, EntityRepository<? extends Entity> r2) {
        var r1Parser = new SqlEntityRepositoryParser(r1);
        var r2Parser = new SqlEntityRepositoryParser(r2);

        return Integer.compare(r1Parser.getOrder(), r2Parser.getOrder());
    }

}
