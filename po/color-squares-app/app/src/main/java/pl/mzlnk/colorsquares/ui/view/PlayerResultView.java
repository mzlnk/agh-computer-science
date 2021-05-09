package pl.mzlnk.colorsquares.ui.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.TextView;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.GameResult;

public class PlayerResultView extends CustomLinearLayout {

    private View playerColor;
    private TextView playerArea;
    private TextView playerName;

    public PlayerResultView(Context context) {
        this(context, null);
    }

    public PlayerResultView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    @Override
    protected void inflate() {
        inflate(this.getContext(), R.layout.v_player_result, this);
    }

    @Override
    protected void loadViewsFromXml() {
        playerColor = this.findViewById(R.id.v_player_result_view_color);
        playerName = this.findViewById(R.id.v_player_result_text_player_name);
        playerArea = this.findViewById(R.id.v_player_result_text_player_area);
    }

    public void setPlayerResult(GameResult.GamePlayerResult gamePlayerResult) {
        playerColor.setBackgroundColor(gamePlayerResult.getPlayer().getColor());
        playerName.setText(gamePlayerResult.getPlayer().getName());
        playerArea.setText(String.valueOf(gamePlayerResult.getArea() + "%"));
    }


}
