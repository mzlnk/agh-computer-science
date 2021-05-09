package pl.mzlnk.emergencyspot.ui.view.menu;

import android.view.View;

public interface SelectableItem {

    void setItemSelected(boolean selected);
    void setOnClickListener(View.OnClickListener listener);

}
