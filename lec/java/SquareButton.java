import javax.swing.JButton;

public class SquareButton extends JButton {
    public SquareButton(String title) {
        super(title);
    }
    public SquareButton() {
        super();
    }
    
    // have to make assumptions about how JButton is coded in order for this to work correctly:
    // 1. must always use getWidth() & getHeight() instead of directly accessing the fields
    @Override
    public int getHeight() {
        return this.getWidth();
    }

    @Override
    public int getWidth() {
        if (super.getWidth() < super.getHeight())
            return super.getWidth();
        else
            return super.getHeight();
    }
}
