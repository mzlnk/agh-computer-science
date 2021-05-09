package pl.mzlnk.conferencegenerator.generator.sql.impl;

import lombok.AllArgsConstructor;
import pl.mzlnk.conferencegenerator.model.entity.Entity;
import pl.mzlnk.conferencegenerator.generator.sql.SqlGenerator;
import pl.mzlnk.conferencegenerator.generator.sql.utils.comparator.EntityRepositoryDependencyComparator;
import pl.mzlnk.conferencegenerator.properties.ConferenceGeneratorProperties;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepositories;
import pl.mzlnk.conferencegenerator.repository.entity.EntityRepository;
import pl.mzlnk.conferencegenerator.service.FileService;

import java.util.List;

@AllArgsConstructor
public abstract class BaseSqlGenerator implements SqlGenerator {

    private static final String NEW_LINE = "\n";

    private static final String RESULT_FILE = "result.sql";

    private FileService fileService;
    private EntityRepositories entityRepositories;

    protected ConferenceGeneratorProperties properties;

    private EntityRepositoryDependencyComparator comparator = new EntityRepositoryDependencyComparator();
    private StringBuilder builder = new StringBuilder();

    public BaseSqlGenerator(FileService fileService, EntityRepositories entityRepositories, ConferenceGeneratorProperties properties) {
        this.fileService = fileService;
        this.entityRepositories = entityRepositories;
        this.properties = properties;
    }

    @Override
    public final void generateSqlFile() {
        removeOldFileIfNecessary();

        generateUseDatabaseSql();
        lineSeparator();

        generateDropTableSql();
        lineSeparator();

        generateCreateTableSql();
        lineSeparator();

        generateInsertIntoSql();

        exportDataToFile();
    }

    protected abstract String useDatabaseSql();

    protected abstract String dropTableSql(EntityRepository<? extends Entity> repository);

    protected abstract String createTableSql(EntityRepository<? extends Entity> repository);

    protected abstract String insertIntoSql(Entity entity);

    private void generateUseDatabaseSql() {
        this.builder
                .append(useDatabaseSql())
                .append(NEW_LINE);
    }

    private void generateDropTableSql() {
        this.entityRepositories.getAllRepositories()
                .stream()
                .sorted(this.comparator.reversed())
                .forEach(repository -> {
                    this.builder
                            .append(dropTableSql(repository))
                            .append(NEW_LINE);
                });
    }

    private void generateCreateTableSql() {
        this.entityRepositories.getAllRepositories()
                .stream()
                .sorted(this.comparator)
                .forEach(repository -> {
                    this.builder
                            .append(createTableSql(repository))
                            .append(NEW_LINE)
                            .append(NEW_LINE);
                });
    }

    private void generateInsertIntoSql() {
        this.entityRepositories.getAllRepositories()
                .stream()
                .sorted(this.comparator)
                .map(EntityRepository::findAll)
                .flatMap(List::stream)
                .forEach(entity -> {
                    this.builder
                            .append(insertIntoSql(entity))
                            .append(NEW_LINE);
                });
    }

    private void removeOldFileIfNecessary() {
        fileService.findFile(FileService.Directory.RESULT, RESULT_FILE).ifPresent(file -> {
            file.delete();
        });
    }

    private void exportDataToFile() {
        fileService.createOrUpdateFile(FileService.Directory.RESULT, RESULT_FILE, builder.toString());
    }

    private void lineSeparator() {
        this.builder
                .append(NEW_LINE)
                .append(NEW_LINE);
    }

}
