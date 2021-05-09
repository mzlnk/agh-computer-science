package pl.mzlnk.colorsquares.ui.fragment.game;

import android.content.Intent;
import android.os.Handler;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.Toast;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.Game;
import pl.mzlnk.colorsquares.domain.game.GameObserver;
import pl.mzlnk.colorsquares.ui.activity.MainActivity;
import pl.mzlnk.colorsquares.ui.activity.StartActivity;
import pl.mzlnk.colorsquares.ui.dialog.GameResultDialog;
import pl.mzlnk.colorsquares.ui.fragment.BaseFragment;
import pl.mzlnk.colorsquares.ui.view.MapCanvas;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameFragment extends BaseFragment implements GameObserver {

    private LinearLayout mainLayout;

    private RelativeLayout grid;
    private Button playAgain;

    private MapCanvas map;

    @Override
    public void onDestroy() {
        super.onDestroy();
        app.currentGame.cancelGame();
        app.currentGame.unregisterObserver(map);
        app.currentGame.unregisterObserver(this);
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_game;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        mainLayout = rootView.findViewById(R.id.f_game_layout_main);
        grid = rootView.findViewById(R.id.f_game_layout_grid);
        playAgain = rootView.findViewById(R.id.f_game_button_play_again);
    }

    @Override
    protected void prepareAnimations() {
        Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.fade_in);
        mainLayout.setAnimation(animation);
    }

    @Override
    protected void prepareViews() {
        map = new MapCanvas(getContext(), app.currentGame.getGameMap());
        grid.addView(map);

        app.currentGame.registerObserver(map);
        app.currentGame.registerObserver(this);
        app.currentGame.startGame();

        showCountdown();
    }

    @Override
    protected void prepareListeners() {
        playAgain.setOnClickListener(view -> {
            Intent intent = new Intent(this.getActivity(), StartActivity.class);
            this.getActivity().startActivity(intent);
            this.getActivity().overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
        });
    }

    @Override
    public void onGameFinished(Game game) {
        MainActivity.mainActivity.runOnUiThread(() -> {
            GameResultDialog dialog = new GameResultDialog(getContext());
            dialog.show();

            playAgain.setVisibility(View.VISIBLE);
        });
    }

    private void showCountdown() {
        Handler handler = new Handler();
        handler.postDelayed(() -> Toast.makeText(getContext(), "3", Toast.LENGTH_SHORT).show(), 100L);
        handler.postDelayed(() -> Toast.makeText(getContext(), "2", Toast.LENGTH_SHORT).show(), 1000L);
        handler.postDelayed(() -> Toast.makeText(getContext(), "1", Toast.LENGTH_SHORT).show(), 2000L);
    }

}
