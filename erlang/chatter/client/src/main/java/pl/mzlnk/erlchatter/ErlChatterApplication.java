package pl.mzlnk.erlchatter;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import pl.mzlnk.erlchatter.ddd.network.NetworkService;
import pl.mzlnk.erlchatter.ddd.user.UserService;

public class ErlChatterApplication extends Application {

    public static void start(String[] args) {
        launch(args);
    }

    public static ErlChatterApplication app;

    public NetworkService networkService;
    public UserService userService;

    public ErlChatterApplication() {
        app = this;
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        createContext();

        Parent root = FXMLLoader.load(getClass().getResource("/welcome.fxml"));
        primaryStage.setTitle("Erl Chatter");
        primaryStage.setScene(new Scene(root, 600, 800));
        primaryStage.setResizable(false);
        primaryStage.show();

        System.out.println("Application started");
    }

    private void createContext() throws Exception {
        this.networkService = new NetworkService();
        this.userService = new UserService();

        this.networkService.start();
    }

}
