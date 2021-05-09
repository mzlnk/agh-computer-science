package pl.mzlnk.colorsquares.ui.fragment.gamesetup;

import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.LinearLayout;

import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentStatePagerAdapter;
import androidx.viewpager.widget.PagerAdapter;
import androidx.viewpager.widget.ViewPager;

import java.util.List;

import pl.mzlnk.colorsquares.R;
import pl.mzlnk.colorsquares.domain.game.Game;
import pl.mzlnk.colorsquares.domain.maptemplate.MapTemplate;
import pl.mzlnk.colorsquares.ui.fragment.BaseFragment;

import static pl.mzlnk.colorsquares.ColorSquaresApp.app;

public class GameSetupMapFragment extends BaseFragment {

    private LinearLayout mainLayout;

    private ViewPager pager;
    private PagerAdapter pagerAdapter;

    private Button submit;

    private List<MapTemplate> allMapTemplates;
    private MapTemplate chosenMapTemplate;

    public GameSetupMapFragment() {
        // required empty constructor
    }

    @Override
    protected int getInflatedLayoutResId() {
        return R.layout.f_game_setup_map;
    }

    @Override
    protected void loadViewsFromXml(View rootView) {
        mainLayout = rootView.findViewById(R.id.f_game_setup_map_layout_main);
        pager = rootView.findViewById(R.id.f_game_setup_map_pager_maps);
        submit = rootView.findViewById(R.id.f_game_setup_map_submit);
    }

    @Override
    protected void prepareAnimations() {
        Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.fade_in);
        mainLayout.setAnimation(animation);
    }

    @Override
    protected void prepareViews() {
        preparePagerAdapter();
    }

    @Override
    protected void prepareData() {
        allMapTemplates = app.mapTemplateRepository.findAll();
        chosenMapTemplate = allMapTemplates.get(0);
    }

    @Override
    protected void prepareListeners() {
        submit.setOnClickListener(view -> {
            app.currentGame = Game.fromMapTemplate(chosenMapTemplate);

            FragmentManager fragmentManager = getFragmentManager();
            fragmentManager
                    .beginTransaction()
                    .replace(R.id.layout_for_fragments, new GameSetupOpponentsFragment())
                    .commit();
        });

        pager.addOnPageChangeListener(new ViewPager.OnPageChangeListener() {

            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {

            }

            @Override
            public void onPageSelected(int position) {
                chosenMapTemplate = allMapTemplates.get(position);
            }

            @Override
            public void onPageScrollStateChanged(int state) {

            }
        });
    }

    private void preparePagerAdapter() {
        pagerAdapter = new MapExamplePagerAdapter(this.getFragmentManager());
        pager.setAdapter(pagerAdapter);
    }

    private final class MapExamplePagerAdapter extends FragmentStatePagerAdapter {

        public MapExamplePagerAdapter(FragmentManager fragmentManager) {
            super(fragmentManager);
        }

        @Override
        public Fragment getItem(int position) {
            return GameSetupMapExampleFragment.newInstance(allMapTemplates.get(position).getId());
        }

        @Override
        public int getCount() {
            return allMapTemplates.size();
        }

    }


}
