package pl.mzlnk.colorsquares.ui.dialog;

import android.content.Context;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.LinearLayout;
import android.widget.TextView;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.GameResult;
import pl.mzlnk.colorsquares.ui.view.PlayerResultView;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameResultDialog extends BaseDialog {

    private LinearLayout mainLayout;

    private TextView winnerName;
    private TextView winnerArea;

    private LinearLayout allPlayers;

    private GameResult gameResult;

    public GameResultDialog(Context context) {
        super(context);
    }

    @Override
    protected int getContentViewResId() {
        return R.layout.d_game_result;
    }

    @Override
    protected void prepareData() {
        gameResult = app.currentGame.getGameResult();
    }

    @Override
    protected void loadViewsFromXml() {
        mainLayout = this.findViewById(R.id.d_game_result_layout_main);

        winnerName = this.findViewById(R.id.d_game_result_text_winner_name);
        winnerArea = this.findViewById(R.id.d_game_result_text_winner_area);
        allPlayers = this.findViewById(R.id.d_game_result_layout_all_players);
    }

    @Override
    protected void prepareAnimations() {
        Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.fade_in);
        mainLayout.setAnimation(animation);
    }

    @Override
    protected void prepareViews() {
        GameResult.GamePlayerResult winner = gameResult.getResults().get(0);

        winnerName.setText(winner.getPlayer().getName());
        winnerName.setTextColor(winner.getPlayer().getColor());
        winnerArea.setText(String.valueOf(gameResult.getResults().get(0).getArea() + "%"));

        for (GameResult.GamePlayerResult gamePlayerResult : gameResult.getResults()) {
            PlayerResultView view = new PlayerResultView(getContext());
            view.setPlayerResult(gamePlayerResult);

            allPlayers.addView(view);
        }
    }

}