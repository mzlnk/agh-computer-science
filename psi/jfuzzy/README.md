#jFuzzyLogic

## About

Author: Marcin Zielonka

## Build project:

If jFuzzyLogic library is not found on remote maven repository, run following command from project root:
```text
mvn install:install-file -Dfile=lib/jFuzzyLogic-1.2.1.jar -DgroupId=net.sourceforge.jFuzzyLogic -DartifactId=jFuzzyLogic -Dversion=1.2.1 -Dpackaging=jar -DgeneratePom=true
```

To build project into executable JAR file - run following command:
```text
mvn clean package
```

## Run application:

To run application - run following command:
```text
java -jar target/jfuzzy.jar fcl/jfuzzy.fcl [roof_slope] [roof_direction] [shadiness] [day_of_year]
```