package pl.edu.agh.ki.psi.jfuzzy;

import net.sourceforge.jFuzzyLogic.FIS;
import net.sourceforge.jFuzzyLogic.rule.FuzzyRuleSet;

public class Application {

    public static void main(String[] args) {
        final FuzzyProperties properties = FuzzyProperties.fromArguments(args);
        System.out.println(properties);

        FIS fis = FIS.load(properties.getFclFilename(), false);

        // wyswietl wykresy funkcji fuzyfikacji i defuzyfikacji
        FuzzyRuleSet fuzzyRuleSet = fis.getFuzzyRuleSet();
        fuzzyRuleSet.chart();

        // zadaj wartosci wejsciowe
        fuzzyRuleSet.setVariable("nachylenie_dachu", properties.getRoofSlope());
        fuzzyRuleSet.setVariable("kierunek_nachylenia_dachu", properties.getRoofDirection());
        fuzzyRuleSet.setVariable("zacienienie", properties.getShadiness());
        fuzzyRuleSet.setVariable("dzien_roku", properties.getDayOfYear());

        // logika sterownika
        fuzzyRuleSet.evaluate();

        // graficzna prezentacja wyjscia
        fuzzyRuleSet.getVariable("przewidywana_efektywnosc").chartDefuzzifier(true);
    }

}
