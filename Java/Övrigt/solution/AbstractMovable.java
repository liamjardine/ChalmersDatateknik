package ex2asteroids.solution;

public abstract class AbstractMovable extends AbstractPositionable {

    private double dx;
    private double dy;

    public AbstractMovable(double x, double y, double width, double height, double dx, double dy) {
        super(x, y, width, height);
        this.dx = dx;
        this.dy = dy;
    }

    public void move() {
        setX(getX() + dx);
        setY(getY() + dy);
    }

    public void stop() {
        dx = dy = 0;
    }

    public void setDx(double dx) {
        this.dx = dx;
    }

    public void setDy(double dy) {
        this.dy = dy;
    }
}
