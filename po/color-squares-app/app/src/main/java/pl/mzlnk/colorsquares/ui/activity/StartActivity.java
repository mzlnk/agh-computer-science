package pl.mzlnk.colorsquares.ui.activity;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;

import androidx.appcompat.app.AppCompatActivity;

import pl.mzlnk.colorsquares.R;


public class StartActivity extends AppCompatActivity {

    private Button start;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.a_start);

        start = findViewById(R.id.a_start_btn_start);

        addListeners();
    }

    @Override
    public void onBackPressed() {
    }

    private void addListeners() {
        start.setOnClickListener(view -> {
            Intent intent = new Intent(StartActivity.this, MainActivity.class);
            StartActivity.this.startActivity(intent);
            StartActivity.this.overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        });
    }

}
