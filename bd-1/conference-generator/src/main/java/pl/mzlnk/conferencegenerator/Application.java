package pl.mzlnk.conferencegenerator;

public abstract class Application {

    public Application() {
        System.out.println(welcomeTitle());
        init();
    }

    protected abstract String welcomeTitle();

    protected void init() {

    }

    protected void run() {

    }

}
