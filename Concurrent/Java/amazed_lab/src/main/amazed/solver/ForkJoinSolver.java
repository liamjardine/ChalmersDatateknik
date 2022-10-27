package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Stack;
import java.util.concurrent.*;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.RecursiveTask;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
        extends SequentialSolver
{
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    static ConcurrentSkipListSet<Integer> visited;
    static AtomicBoolean found;
    static ForkJoinPool pool;

    int steps;
    List<ForkJoinSolver> children;

    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        found = new AtomicBoolean();

        visited = new ConcurrentSkipListSet<Integer>();
        pool= new ForkJoinPool();
        steps = 0;
        children = new ArrayList();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        super(maze);
        this.forkAfter = forkAfter;
        found = new AtomicBoolean();

        visited = new ConcurrentSkipListSet<Integer>();
        steps = 0;
        pool= new ForkJoinPool();
        children = new ArrayList();

    }


    //create helper solver
    public ForkJoinSolver(Maze maze, int forkAfter, int start){
        super(maze);
        this.forkAfter = forkAfter;

        int steps = 0;
        this.start = start;
        children = new ArrayList();

    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        int player = 0;
        int current = 0;
        if(visited.add(start)){ //only initially create the player if start is still unvisited
            player = maze.newPlayer(start);
            frontier.push(start);
        }

        /* loop won't be entered if player not created: frontier would initally be empty
         */
        while( (!(frontier.empty()) ) && !(found.get()) ){

            current = frontier.pop();
            if(current == start || visited.add(current)){
                maze.move(player, current);
                steps++;
                if(maze.hasGoal(current)){
                    found.set(true);
                    return pathFromTo(start, current);
                }
                for (int nb: maze.neighbors(current)){
                    if(!visited.contains(nb)){
                        frontier.add(nb);
                    }
                    if(!visited.contains(nb)){
                        predecessor.put(nb,current);
                    }


                }
                if ( frontier.size()>1 && steps>=forkAfter){ // set to not fork if frontier has only one path to search but also no earlier than after forkAfter steps
                    steps=0;
                    ForkJoinSolver child = new ForkJoinSolver(maze, forkAfter, frontier.pop());
                    children.add(child);
                    child.fork();

                }
            }

        }
        //wait for eventual children
        if(children!=null){
            for(ForkJoinSolver child: children){
                List<Integer> recieved = null;
                recieved = child.join();
                if (recieved!=null){ // if not null it's a path to a goal
                    List<Integer> connectBranch=pathFromTo(start, recieved.get(0));
                    connectBranch.remove(connectBranch.size() -1);
                    connectBranch.addAll(recieved);
                    return (connectBranch);
                }
            }
        }

        return null;
    }
}