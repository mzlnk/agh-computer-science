package pl.mzlnk.emergencyspot.ui.viewprovider;

import android.content.Context;

import androidx.annotation.Nullable;
import androidx.fragment.app.FragmentManager;

import java.util.AbstractMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.model.hospital.HospitalDto;
import pl.mzlnk.emergencyspot.ui.fragment.HospitalWardsOverviewFragment;
import pl.mzlnk.emergencyspot.ui.view.list.HospitalListItemView;

public class HospitalsOverviewViewProvider implements ViewProvider<HospitalDto, Long, HospitalListItemView> {

    @Override
    public List<Map.Entry<Long, HospitalListItemView>> provide(List<HospitalDto> data,
                                                               Context context,
                                                               @Nullable FragmentManager fragmentManager) {
        return data
                .stream()
                .map(hospital -> {
                    HospitalListItemView view = new HospitalListItemView(context);
                    view.setDetails(hospital);

                    view.setOnClickListener(v -> {
                        fragmentManager
                                .beginTransaction()
                                .replace(R.id.a_main_fragment_container, HospitalWardsOverviewFragment.newInstance(hospital.getId()))
                                .commit();
                    });

                    return new AbstractMap.SimpleEntry<>(1L, view);
                })
                .collect(Collectors.toList());
    }

}
