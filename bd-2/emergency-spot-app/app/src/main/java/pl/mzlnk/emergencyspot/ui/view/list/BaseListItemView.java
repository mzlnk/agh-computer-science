package pl.mzlnk.emergencyspot.ui.view.list;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

import androidx.annotation.Nullable;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.view.BaseLinearLayoutView;
import pl.mzlnk.emergencyspot.ui.view.button.NoIconButton;

public abstract class BaseListItemView extends BaseLinearLayoutView {

    private TextView title;
    private TextView subtitle;
    private NoIconButton submit;

    public BaseListItemView(Context context) {
        this(context, null);
    }

    public BaseListItemView(Context context, AttributeSet attributeSet) {
        super(context, attributeSet, R.attr.ListItemViewStyle);
    }

    @Override
    protected int xmlLayoutResId() {
        return R.layout.v_list_item;
    }

    @Override
    protected void loadViewsFromXml() {
        this.title = findViewById(R.id.v_list_item_text_title);
        this.subtitle = findViewById(R.id.v_list_item_text_subtitle);
        this.submit = findViewById(R.id.v_list_item_btn_submit);
    }

    @Override
    public final void setOnClickListener(@Nullable OnClickListener listener) {
        this.submit.setOnClickListener(listener);
    }

    protected void setTitle(String title) {
        this.title.setText(title);
    }

    protected void setSubtitle(String subtitle) {
        this.subtitle.setText(subtitle);
    }

    protected void setButtonLabel(String label) {
        this.submit.setLabel(label);
    }

}
