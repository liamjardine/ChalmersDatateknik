package ex2asteroids.solution;

/*
    Class representing a Spaceship

 */
public class Spaceship extends AbstractMovable {
    public static final double MAX_DX = 2;
    public static final double MAX_DY = 2;

    public Spaceship(double x, double y, double width, double height, double dx, double dy) {
        super(x, y, width, height, dx, dy);
    }

    public Spaceship(double x, double y, double width, double height) {
        this(x, y, width, height, 0, 0);
    }

}
