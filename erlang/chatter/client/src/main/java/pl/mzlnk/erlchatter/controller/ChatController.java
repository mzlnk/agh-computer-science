package pl.mzlnk.erlchatter.controller;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import lombok.SneakyThrows;
import pl.mzlnk.erlchatter.controller.utils.AlertUtil;
import pl.mzlnk.erlchatter.ddd.network.NetworkResponseObserver;
import pl.mzlnk.erlchatter.ddd.network.request.MessageAllRequest;
import pl.mzlnk.erlchatter.ddd.network.request.SignOutRequest;
import pl.mzlnk.erlchatter.ddd.network.response.MessageAllResponse;
import pl.mzlnk.erlchatter.ddd.network.response.SignOutResponse;
import pl.mzlnk.erlchatter.ddd.network.response.UserJoinResponse;
import pl.mzlnk.erlchatter.ddd.network.response.UserLeaveResponse;

import java.io.IOException;
import java.text.SimpleDateFormat;

import static pl.mzlnk.erlchatter.ErlChatterApplication.app;

public class ChatController implements NetworkResponseObserver {

    private static final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    public ChatController() {
        app.networkService.addResponseObserver(this);
    }

    @FXML
    private VBox chatContainer;

    @FXML
    private TextField messageField;

    @FXML
    private Button sendButton, signOutButton;

    @FXML
    protected void onMessageSend(ActionEvent event) {
        app.networkService.sendRequest(new MessageAllRequest(messageField.getText(), app.userService.getUser()));
    }

    @FXML
    protected void onSignOut(ActionEvent event) {
        app.networkService.sendRequest(new SignOutRequest(app.userService.getUser()));
    }

    @Override
    public void onResponse(MessageAllResponse response) {
        Platform.runLater(() -> {
            Label label = new Label(String.format(
                    "[%s] %s >> %s",
                    sdf.format(response.getDate().getTime()),
                    response.getFrom(),
                    response.getMessage()
            ));

            chatContainer.getChildren().add(label);
        });
    }

    @Override
    public void onResponse(UserLeaveResponse response) {
        Platform.runLater(() -> {
            Label label = new Label(String.format(
                    "[%s] %s left",
                    sdf.format(response.getDate().getTime()),
                    response.getWho()
            ));

            chatContainer.getChildren().add(label);
        });
    }

    @Override
    public void onResponse(UserJoinResponse response) {
        Platform.runLater(() -> {
            Label label = new Label(String.format(
                    "[%s] %s joined",
                    sdf.format(response.getDate().getTime()),
                    response.getWho()
            ));

            chatContainer.getChildren().add(label);
        });
    }

    @Override
    @SneakyThrows
    public void onResponse(SignOutResponse response) {
        Platform.runLater(() -> {
            try {
                app.userService.signOut();

                Stage stage = (Stage) signOutButton.getScene().getWindow();
                Parent root = FXMLLoader.load(getClass().getResource("/welcome.fxml"));

                stage.setScene(new Scene(root, 600, 800));
                stage.show();

                AlertUtil.showAlert(
                        Alert.AlertType.INFORMATION,
                        stage,
                        "Erl Chatter",
                        "You left the chat!"
                );
            } catch (IOException e) {
                e.printStackTrace();
            }

        });
    }
}
