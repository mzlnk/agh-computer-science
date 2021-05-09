package pl.mzlnk.conferencegenerator.generator.sql.impl.mssql;

import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.generator.sql.impl.BaseSqlGenerator;
import pl.mzlnk.conferencegenerator.generator.sql.utils.parser.SqlEntityFieldParser;
import pl.mzlnk.conferencegenerator.generator.sql.utils.parser.SqlEntityRepositoryParser;
import pl.mzlnk.conferencegenerator.properties.ConferenceGeneratorProperties;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepository;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MsSqlGenerator extends BaseSqlGenerator {

    private static final String USE_SQL_PATTERN = "USE {db_name};";
    private static final String DROP_TABLE_SQL_PATTERN = "DROP TABLE IF EXISTS {db_scheme}.{db_table};";
    private static final String CREATE_TABLE_SQL_PATTERN = "CREATE TABLE {db_scheme}.{db_table} ( \n\t{data}\n);";
    private static final String INSERT_INTO_SQL_PATTERN = "INSERT INTO {db_scheme}.{db_table} ( {columns} ) VALUES ( {values} );";

    private static final String DATA_TYPE_SQL_PATTERN = "{column}{data_type}{primary_key}{auto_increment}{not_null}";
    private static final String FOREIGN_KEY_SQL_PATTERN = "FOREIGN KEY ({column_name}) REFERENCES {db_scheme}.{foreign_table} ({foreign_column})";

    private static final String PRIMARY_KEY = "PRIMARY KEY";
    private static final String AUTO_INCREMENT = "IDENTITY (1, 1)";
    private static final String NOT_NULL = "NOT NULL";

    private static final String SINGLE_SPACE = " ";

    private static final String DATA_TYPE_DELIMITER = ",\n\t";

    private static final String COMMA = ",";

    public MsSqlGenerator(FileService fileService, EntityRepositories entityRepositories, ConferenceGeneratorProperties properties) {
        super(fileService, entityRepositories, properties);
    }

    @Override
    protected String useDatabaseSql() {
        return USE_SQL_PATTERN
                .replace("{db_name}", properties.getDatabaseName());
    }

    @Override
    protected String dropTableSql(EntityRepository<? extends Entity> repository) {
        var parser = new SqlEntityRepositoryParser(repository.getEntityType());

        return DROP_TABLE_SQL_PATTERN
                .replace("{db_scheme}", properties.getDatabaseScheme())
                .replace("{db_table}", parser.getTableName());
    }

    @Override
    protected String createTableSql(EntityRepository<? extends Entity> repository) {
        var repositoryParser = new SqlEntityRepositoryParser(repository.getEntityType());

        String sql = CREATE_TABLE_SQL_PATTERN;

        sql = sql.replace("{db_scheme}", properties.getDatabaseScheme());
        sql = sql.replace("{db_table}", repositoryParser.getTableName());

        List<String> data = new ArrayList<>();

        data.addAll(
                Stream.of(repositoryParser.getFields())
                        .map(SqlEntityFieldParser::new)
                        .filter(SqlEntityFieldParser::isColumn)
                        .map(parser -> {
                            return DATA_TYPE_SQL_PATTERN
                                    .replace("{column}", parser.getColumnName())
                                    .replace("{data_type}", SINGLE_SPACE + MsSqlDataTypeEnum.fromClass(parser.getColumnType()).getSqlDataType())
                                    .replace("{primary_key}", parser.isPrimaryKey() ? (SINGLE_SPACE + PRIMARY_KEY) : "")
                                    .replace("{auto_increment}", parser.isAutoIncrement() ? (SINGLE_SPACE + AUTO_INCREMENT) : "")
                                    .replace("{not_null}", parser.isNotNull() ? (SINGLE_SPACE + NOT_NULL) : "");
                        })
                        .collect(Collectors.toList())
        );

        data.addAll(
                Stream.of(repositoryParser.getFields())
                        .map(SqlEntityFieldParser::new)
                        .filter(SqlEntityFieldParser::isColumn)
                        .filter(SqlEntityFieldParser::isForeignKey)
                        .map(parser -> {
                            return FOREIGN_KEY_SQL_PATTERN
                                    .replace("{column_name}", parser.getColumnName())
                                    .replace("{db_scheme}", properties.getDatabaseScheme())
                                    .replace("{foreign_table}", parser.getForeignTableName())
                                    .replace("{foreign_column}", parser.getForeignColumnName());
                        })
                        .collect(Collectors.toList())
        );

        sql = sql.replace("{data}", String.join(DATA_TYPE_DELIMITER, data));

        return sql;
    }

    @Override
    protected String insertIntoSql(Entity entity) {
        var repositoryParser = new SqlEntityRepositoryParser(entity.getEntityType());

        String sql = INSERT_INTO_SQL_PATTERN;

        sql = sql.replace("{db_scheme}", properties.getDatabaseScheme());
        sql = sql.replace("{db_table}", repositoryParser.getTableName());

        List<String> columns = Stream.of(entity.getClass().getDeclaredFields())
                .map(field -> new SqlEntityFieldParser(field, entity))
                .filter(SqlEntityFieldParser::isColumn)
                .filter(p -> !p.isAutoIncrement())
                .map(SqlEntityFieldParser::getColumnName)
                .collect(Collectors.toList());

        List<String> values = Stream.of(entity.getClass().getDeclaredFields())
                .map(field -> new SqlEntityFieldParser(field, entity))
                .filter(SqlEntityFieldParser::isColumn)
                .filter(p -> !p.isAutoIncrement())
                .map(SqlEntityFieldParser::getStringValue)
                .collect(Collectors.toList());

        sql = sql.replace("{columns}", String.join(COMMA, columns));
        sql = sql.replace("{values}", String.join(COMMA, values));

        return sql;
    }

}
