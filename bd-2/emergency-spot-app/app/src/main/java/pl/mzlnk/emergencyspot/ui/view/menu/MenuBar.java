package pl.mzlnk.emergencyspot.ui.view.menu;

import android.content.Context;
import android.util.AttributeSet;

import pl.mzlnk.emergencyspot.R;
import pl.mzlnk.emergencyspot.ui.view.BaseLinearLayoutView;

public class MenuBar extends BaseLinearLayoutView {

    private MenuItem addPatientItem, hospitalsItem, mapItem, hospitalStaysItem, profileItem;

    private SelectableItem currentlySelected = mapItem;

    public MenuBar(Context context) {
        super(context);
    }

    public MenuBar(Context context, AttributeSet attributeSet) {
        super(context, attributeSet);
    }

    protected int xmlLayoutResId() {
        return R.layout.v_menu_bar;
    }

    protected void loadViewsFromXml() {
        this.mapItem = findViewById(R.id.v_menu_bar_menu_item_map);
        this.addPatientItem = findViewById(R.id.v_menu_bar_menu_item_add_patient);
        this.hospitalsItem = findViewById(R.id.v_menu_bar_menu_item_hospitals);
        this.hospitalStaysItem = findViewById(R.id.v_menu_bar_menu_item_hospital_stays);
        this.profileItem = findViewById(R.id.v_menu_bar_menu_item_profile);
    }

    public final void setAddPatientItemOnClickListener(final OnClickListener listener) {
        setOnClickListener(listener, this.addPatientItem);
    }

    public final void setHospitalsItemOnClickListener(final OnClickListener listener) {
        setOnClickListener(listener, this.hospitalsItem);
    }

    public final void setMapItemOnClickListener(final OnClickListener listener) {
        setOnClickListener(listener, this.mapItem);
    }

    public final void setHospitalStaysItemOnClickListener(final OnClickListener listener) {
        setOnClickListener(listener, this.hospitalStaysItem);
    }

    public final void setProfileItemOnClickListener(final OnClickListener listener) {
        setOnClickListener(listener, this.profileItem);
    }

    private void setOnClickListener(OnClickListener listener, SelectableItem item) {
        item.setOnClickListener(view -> {
            listener.onClick(view);
            setItemSelected(item);
        });
    }

    private void setItemSelected(SelectableItem itemSelected) {
        switchItemSelect(this.addPatientItem, itemSelected);
        switchItemSelect(this.mapItem, itemSelected);
        switchItemSelect(this.hospitalsItem, itemSelected);
        switchItemSelect(this.hospitalStaysItem, itemSelected);
        switchItemSelect(this.profileItem, itemSelected);

        this.currentlySelected = itemSelected;
    }

    private void switchItemSelect(SelectableItem item, SelectableItem selected) {
        item.setItemSelected(item == selected);
    }

}
