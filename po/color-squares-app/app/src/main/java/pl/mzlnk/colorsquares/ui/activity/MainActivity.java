package pl.mzlnk.colorsquares.ui.activity;

import android.content.Intent;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.FragmentManager;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.ui.fragment.gamesetup.GameSetupMapFragment;

public class MainActivity extends AppCompatActivity {

    public static MainActivity mainActivity;

    public MainActivity() {
        mainActivity = this;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.a_main);
        showGameSetupFragment();
    }

    @Override
    public void onBackPressed() {
        Intent intent = new Intent(this, StartActivity.class);
        startActivity(intent);
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
    }

    private void showGameSetupFragment() {
        FragmentManager fragmentManager = getSupportFragmentManager();
        fragmentManager
                .beginTransaction()
                .replace(R.id.layout_for_fragments, new GameSetupMapFragment())
                .commit();
    }

}
