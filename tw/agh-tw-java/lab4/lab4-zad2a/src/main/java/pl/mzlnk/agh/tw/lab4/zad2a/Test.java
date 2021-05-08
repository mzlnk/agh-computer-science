package pl.mzlnk.agh.tw.lab4.zad2a;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class Test {

    public static void main(String[] args) {

        try(BufferedWriter writer = new BufferedWriter(new FileWriter("filename"))) {
            writer.write("some text");
        } catch(IOException e) {

        }

    }

}
