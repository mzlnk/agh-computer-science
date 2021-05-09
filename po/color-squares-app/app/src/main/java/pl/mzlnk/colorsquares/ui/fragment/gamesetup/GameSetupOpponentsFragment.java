package pl.mzlnk.colorsquares.ui.fragment.gamesetup;

import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.TextView;

import androidx.fragment.app.FragmentManager;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.Game;
import pl.mzlnk.colorsquares.domain.game.GamePlayer;
import pl.mzlnk.colorsquares.ui.fragment.BaseFragment;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameSetupOpponentsFragment extends BaseFragment {

    private LinearLayout mainLayout;

    private TextView opponentsPreview;
    private SeekBar opponentsPicker;

    private Button submit;

    public GameSetupOpponentsFragment() {
        // Required empty public constructor
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_game_setup_opponents;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        mainLayout = rootView.findViewById(R.id.f_game_setup_opponent_layout_main);
        opponentsPreview = rootView.findViewById(R.id.f_game_setup_opponents_text_amount_preview);
        opponentsPicker = rootView.findViewById(R.id.f_game_setup_opponents_seekbar_opponents_picker);
        submit = rootView.findViewById(R.id.f_game_setup_opponents_button_submit);
    }

    @Override
    protected void prepareAnimations() {
        Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.fade_in);
        mainLayout.setAnimation(animation);
    }

    @Override
    protected void prepareListeners() {
        opponentsPicker.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
            @Override
            public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                opponentsPreview.setText(String.valueOf(opponentsPicker.getProgress()));
            }

            @Override
            public void onStartTrackingTouch(SeekBar seekBar) {

            }

            @Override
            public void onStopTrackingTouch(SeekBar seekBar) {

            }
        });

        submit.setOnClickListener(view -> {
            Game game = app.currentGame;

            int opponents = opponentsPicker.getProgress();
            for (int i = 0; i < opponents; i++) {
                game.registerGamePlayer(new GamePlayer(2 + i, true));
            }

            FragmentManager fragmentManager = getFragmentManager();
            fragmentManager
                    .beginTransaction()
                    .replace(R.id.layout_for_fragments, new GameSetupPlayerFragment())
                    .commit();
        });
    }

}
