package pl.mzlnk.conferencegenerator.generator.entity.utils.comparator;

import lombok.NoArgsConstructor;
import pl.mzlnk.conferencegenerator.generator.entity.EntityGenerator;
import pl.mzlnk.conferencegenerator.generator.sql.utils.parser.SqlEntityRepositoryParser;

import java.util.Comparator;

@NoArgsConstructor
public class EntityGeneratorDependencyComparator implements Comparator<EntityGenerator> {

    @Override
    public int compare(EntityGenerator e1, EntityGenerator e2) {
        var e1Parser = new SqlEntityRepositoryParser(e1.getEntityType());
        var e2Parser = new SqlEntityRepositoryParser(e2.getEntityType());

        return Integer.compare(e1Parser.getOrder(), e2Parser.getOrder());
    }

}
