package pl.mzlnk.erlchatter.controller;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.stage.Stage;
import pl.mzlnk.erlchatter.controller.utils.AlertUtil;
import pl.mzlnk.erlchatter.ddd.network.NetworkResponseObserver;
import pl.mzlnk.erlchatter.ddd.network.request.SignInRequest;
import pl.mzlnk.erlchatter.ddd.network.request.SignUpRequest;
import pl.mzlnk.erlchatter.ddd.network.response.ErrorResponse;
import pl.mzlnk.erlchatter.ddd.network.response.SignInResponse;
import pl.mzlnk.erlchatter.ddd.network.response.SignUpResponse;

import java.io.IOException;

import static pl.mzlnk.erlchatter.ErlChatterApplication.app;

public class WelcomeController implements NetworkResponseObserver {

    public WelcomeController() {
        app.networkService.addResponseObserver(this);
    }

    @FXML
    private TextField loginField;

    @FXML
    private TextField passwordField;

    @FXML
    private Button signUpButton, signInButton;

    @FXML
    protected void onSignUp(ActionEvent event) {
        app.networkService.sendRequest(new SignUpRequest(loginField.getText(), passwordField.getText()));
    }

    @FXML
    protected void onSignIn(ActionEvent event) {
        app.networkService.sendRequest(new SignInRequest(loginField.getText(), passwordField.getText(), app.networkService.getPid()));
    }

    @Override
    public void onResponse(ErrorResponse response) {
        switch (response.getFrom()) {
            case USER_SIGN_UP, USER_SIGN_IN -> {
                Platform.runLater(() -> {
                    AlertUtil.showAlert(
                            Alert.AlertType.INFORMATION,
                            loginField.getScene().getWindow(),
                            "Error",
                            response.getMessage()
                    );
                });
            }
        }
    }

    @Override
    public void onResponse(SignInResponse response) {
        Platform.runLater(() -> {
            try {
                app.userService.signIn(response.getLogin(), response.getToken());

                Stage stage = (Stage) loginField.getScene().getWindow();
                Parent root = FXMLLoader.load(getClass().getResource("/chat.fxml"));

                stage.setScene(new Scene(root, 600, 800));
                stage.show();

                AlertUtil.showAlert(
                        Alert.AlertType.INFORMATION,
                        stage,
                        "Welcome",
                        "Welcome, " + response.getLogin()
                );
            } catch (IOException e) {
                e.printStackTrace();
            }


        });
    }

    @Override
    public void onResponse(SignUpResponse response) {
        Platform.runLater(() -> {
            AlertUtil.showAlert(
                    Alert.AlertType.INFORMATION,
                    loginField.getScene().getWindow(),
                    "Signed up",
                    "Successfully signed up!"
            );
        });
    }

}
