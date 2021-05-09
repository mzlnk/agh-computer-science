package pl.mzlnk.colorsquares.ui.fragment.gamesetup;

import android.annotation.SuppressLint;
import android.graphics.Color;
import android.graphics.Point;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.fragment.app.FragmentManager;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.GamePlayer;
import pl.mzlnk.colorsquares.ui.fragment.BaseFragment;
import pl.mzlnk.colorsquares.ui.fragment.game.GameFragment;
import pl.mzlnk.colorsquares.ui.view.MapCanvas;
import pl.mzlnk.colorsquares.ui.view.MapPositionView;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameSetupPlayerFragment extends BaseFragment {

    private LinearLayout mainLayout;

    private TextView colorPreview;
    private SeekBar[] colorPickers = new SeekBar[3];

    private RelativeLayout grid;

    private Button start;

    private MapPositionView chosenPositionView;

    private int chosenColor = Color.rgb(128, 128, 128);
    private Point chosenPosition;

    public GameSetupPlayerFragment() {
        // required empty constructor
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_game_setup_player;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        mainLayout = rootView.findViewById(R.id.f_game_setup_player_layout_main);

        colorPreview = rootView.findViewById(R.id.f_game_setup_player_text_color_preview);

        colorPickers[0] = rootView.findViewById(R.id.f_game_setup_player_seekbar_color_picker_1);
        colorPickers[1] = rootView.findViewById(R.id.f_game_setup_player_seekbar_color_picker_2);
        colorPickers[2] = rootView.findViewById(R.id.f_game_setup_player_seekbar_color_picker_3);

        grid = rootView.findViewById(R.id.f_game_setup_player_layout_grid);

        start = rootView.findViewById(R.id.f_game_setup_player_button_start);
    }

    @Override
    protected void prepareAnimations() {
        Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.fade_in);
        mainLayout.setAnimation(animation);
    }

    @Override
    protected void prepareViews() {
        grid.addView(new MapCanvas(this.getContext(), app.currentGame.getGameMap()));
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    protected void prepareListeners() {
        for (SeekBar colorPicker : colorPickers) {
            colorPicker.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {
                @Override
                public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
                    chosenColor = Color.rgb(colorPickers[0].getProgress(), colorPickers[1].getProgress(), colorPickers[2].getProgress());
                    colorPreview.setTextColor(chosenColor);

                    if (chosenPositionView != null) {
                        chosenPositionView.setColor(chosenColor);
                    }
                }

                @Override
                public void onStartTrackingTouch(SeekBar seekBar) {

                }

                @Override
                public void onStopTrackingTouch(SeekBar seekBar) {

                }
            });
        }

        grid.setOnTouchListener((v, event) -> {
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                int size = grid.getWidth();
                int unitSize = size / app.currentGame.getGameMap().getSize();
                int x = (int) (event.getX() / unitSize);
                int y = (int) (event.getY() / unitSize);

                if (app.currentGame.getGameMap().isOccupied(new Point(x, y))) {
                    Toast.makeText(getContext(), "This position is already occupied!", Toast.LENGTH_SHORT).show();
                    return false;
                }

                chosenPosition = new Point(x, y);

                if (chosenPositionView != null) {
                    grid.removeView(chosenPositionView);
                }

                chosenPositionView = new MapPositionView(getContext(), unitSize, chosenPosition);
                chosenPositionView.setColor(chosenColor);
                grid.addView(chosenPositionView);
            }
            return false;
        });

        start.setOnClickListener(view -> {
            if (chosenPosition == null) {
                Toast.makeText(getContext(), "Choose start position first!", Toast.LENGTH_SHORT).show();
                return;
            }

            GamePlayer gamePlayer = new GamePlayer(1, false);
            gamePlayer.setColor(chosenColor);
            gamePlayer.setStartPosition(chosenPosition);

            app.currentGame.registerGamePlayer(gamePlayer);

            FragmentManager fragmentManager = getFragmentManager();
            fragmentManager
                    .beginTransaction()
                    .replace(R.id.layout_for_fragments, new GameFragment())
                    .commit();
        });
    }
}
