package pl.mzlnk.emergencyspot.ui.activity;

import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import com.android.volley.Request;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.JsonRequest;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;

import org.json.JSONObject;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.fragment.AddHospitalPatientFragment;
import pl.mzlnk.emergencyspot.ui.fragment.HospitalStaysOverviewFragment;
import pl.mzlnk.emergencyspot.ui.fragment.HospitalsOverviewFragment;
import pl.mzlnk.emergencyspot.ui.fragment.MapOverviewFragment;
import pl.mzlnk.emergencyspot.ui.fragment.ProfileOverviewFragment;
import pl.mzlnk.emergencyspot.ui.view.menu.MenuBar;

public class MainActivity extends AppCompatActivity {

    private MenuBar menuBar;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.a_main);

        this.menuBar = findViewById(R.id.a_main_menu_bar_menu);

        showFragment(new MapOverviewFragment());
        addMenuItemListeners();
    }

    private void addMenuItemListeners() {
        this.menuBar.setHospitalsItemOnClickListener(view -> showFragment(new HospitalsOverviewFragment()));
        this.menuBar.setHospitalStaysItemOnClickListener(view -> showFragment(new HospitalStaysOverviewFragment()));
        this.menuBar.setMapItemOnClickListener(view -> showFragment(new MapOverviewFragment()));
        this.menuBar.setProfileItemOnClickListener(view -> showFragment(new ProfileOverviewFragment()));
        this.menuBar.setAddPatientItemOnClickListener(view -> showFragment(new AddHospitalPatientFragment()));
    }

    private void showFragment(Fragment fragment) {
        FragmentManager fragmentManager = getSupportFragmentManager();
        fragmentManager
                .beginTransaction()
                .replace(R.id.a_main_fragment_container, fragment)
                .commit();
    }

}
