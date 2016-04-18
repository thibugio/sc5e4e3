// lecture 04-15-2016
// java coding paradigms: a square button
// "If I am the button, how do I make sure that I stay square?" -> "method acting"

import javax.swing.*;

public class Frame345 extends JFrame {
    public Frame345() {
        //JButton button = new JButton("click");
        //button.setPreferredSize(new java.awt.Dimension(300,300));
        JButton button = new SquareButton("click");

        // use default layout manager
        this.getContentPane().add(button, "Center");
        // set the size
        this.pack();
    }

    public static void main(String[] args) {
        new Frame345().setVisible(true);
    }
}
