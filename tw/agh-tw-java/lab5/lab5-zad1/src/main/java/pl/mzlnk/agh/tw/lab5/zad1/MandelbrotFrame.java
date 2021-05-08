package pl.mzlnk.agh.tw.lab5.zad1;

import lombok.extern.slf4j.Slf4j;

import javax.swing.*;
import java.awt.*;

@Slf4j
public class MandelbrotFrame extends JFrame {

    private Mandelbrot mandelbrot;

    public MandelbrotFrame(Mandelbrot mandelbrot) {
        super("Mandelbrot");

        this.mandelbrot = mandelbrot;

        setBounds(100, 100, mandelbrot.getWidth(), mandelbrot.getHeight());
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(mandelbrot.getImage(), 0, 0, this);
    }

}
