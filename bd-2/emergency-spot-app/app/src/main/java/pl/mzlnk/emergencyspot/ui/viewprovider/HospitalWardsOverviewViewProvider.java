package pl.mzlnk.emergencyspot.ui.viewprovider;

import android.content.Context;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;

import java.util.AbstractMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import pl.mzlnk.emergencyspot.model.hospitalward.HospitalWardDto;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalWardListItemView;

public class HospitalWardsOverviewViewProvider implements ViewProvider<HospitalWardDto, Long, HospitalWardListItemView> {

    @Override
    public List<Map.Entry<Long, HospitalWardListItemView>> provide(List<HospitalWardDto> data,
                                                                   Context context,
                                                                   @Nullable FragmentManager fragmentManager) {
        return data
                .stream()
                .map(hospitalWard -> {
                    HospitalWardListItemView view = new HospitalWardListItemView(context);
                    view.setDetails(hospitalWard);

                    view.setOnClickListener(v -> Toast.makeText(context, "Hospital: " + hospitalWard.getWardType().name(), Toast.LENGTH_SHORT).show());

                    return new AbstractMap.SimpleEntry<>(1L, view);
                })
                .collect(Collectors.toList());
    }

}
