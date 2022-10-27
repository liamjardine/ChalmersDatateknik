package ex2asteroids.solution;

import java.util.Random;

/*
    Class representing a single Asteroid

 */
public class Asteroid extends AbstractMovable {



    public Asteroid(double x, double y, double width, double height, double dx, double dy) {
        super(x, y, width, height, dx, dy);
    }

    // TODO
    private static Random rand = new Random();

    public Asteroid(double gameWidth, double gameHeight){
        this(0, 0, 0, 0, 0, 0);
        int rangeX = rand.nextInt((int)gameWidth);
        int rangeY = rand.nextInt((int)gameHeight);

        int dx = rand.nextInt(5) + 1;
        int dy = rand.nextInt(5) + 1;
        switch(rand.nextInt(4)){
            case 0: setDx(dx); setDy(dy); break;
            case 1: setDx(-dx); setDy(dy);break;
            case 3: setDx(dx); setDy(-dy);break;
            default: setDx(-dx); setDy(-dy);
        }

    }



}