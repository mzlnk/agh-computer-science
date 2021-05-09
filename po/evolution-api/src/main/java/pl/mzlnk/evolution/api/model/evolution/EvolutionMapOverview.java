package pl.mzlnk.evolution.api.model.evolution;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import pl.mzlnk.evolution.api.model.entity.Entity;
import pl.mzlnk.evolution.api.model.entity.LivingEntity;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;
import java.util.stream.Collectors;

@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class EvolutionMapOverview {

    public static EvolutionMapOverview fromEvolution(Evolution evolution) {
        EvolutionMapOverview mapOverview = new EvolutionMapOverview();

        mapOverview.setDay(evolution.getCurrentDay());
        evolution.getWorld().findAllEntities()
                .stream()
                .collect(Collectors.toMap(
                        Entity::getLocation,
                        e -> {
                            TreeSet<Entity> set = new TreeSet<>(Comparator.comparingInt(a -> a.getEntityType().getPriority()));
                            set.add(e);
                            return set;
                        },
                        (e1, e2) -> {
                            e1.addAll(e2);
                            return e1;
                        })
                )
                .values()
                .stream()
                .filter(set -> set.size() > 0)
                .map(TreeSet::last)
                .forEach(entity -> mapOverview.addEntity(entity, evolution.getProperties().getMaxAnimalEnergy()));

        return mapOverview;
    }

    @Setter
    private int day;

    private List<Integer> x = new ArrayList<>();
    private List<Integer> y = new ArrayList<>();
    private List<String> type = new ArrayList<>();
    private List<Double> energyFactor = new ArrayList<>();

    private void addEntity(Entity entity, int maxEnergy) {
        this.x.add(entity.getLocation().x);
        this.y.add(entity.getLocation().y);
        this.type.add(entity.getEntityType().getShortcut());

        int energy = 0;
        if (entity instanceof LivingEntity) {
            energy = ((LivingEntity) entity).getEnergy();
        }
        this.energyFactor.add(Math.min(Math.floor((double) energy / maxEnergy * 100) / 100, 1.0));
    }

}
