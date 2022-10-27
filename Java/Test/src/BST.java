/*
import java.io.*;

public class BST<Item extends Comparable<Item>> {
    private class Node {
        private Item item;
        private Node left, right;
        private int height;

        public Node(Item item) {
            this.item = item;
            this.left = left;
            this.right = right;
            height = 1 + Math.max(getHeight(left), getHeight(right));
        }
        static int getHeight (Node node){
            if (node == null)
                return 0;
            else
                return node.height;
        }
        public String toString() {
            String result = "Node(" + item;
            if (left != null)
                result += ", left=" + left;
            if (right != null)
                result += ", left=" + right;
            result += ")";
            return result;

        }
    }

    private Node root = null;

    private Node leftRotate(Node node){
        Node x = node;
        Node y = x.right;
        Node A = x.left;
        Node B = y.left;
        Node C = y.right;

        x.right = B;
        y.left = x;
        return y;
    }

    private Node rightRotate(Node node){
        Node y = node;
        Node x = y.left;
        Node A = x.left;
        Node B = y.right;
        Node C = y.right;

        y.left = B;
        x.right = y;
        return x;
    }

    private Node rebalance(Node node){
        if (node == null) return null;
        int heigthDiff =  getHeigh(node.left) - getHeight(node.right);

        if (heigthDiff == 2){
            int leftDiff = getHeight(node.left.left) - getHeight(node.left.right);
            if (left <= 0){
                node.left = leftRotate(node.left);
            }
        }
        else if (heigthDiff ==-2){

        }
        return node;
    }

    public boolean contains(Item item){
        return contains(root.item);
    }
    private boolean contains(Node node, Item item){
        if (node == null)
            return false;
        else if (item.compareTo(node.item) < 0)
            return contains(node.left, item);
        else if (item.compareTo(node.item) > 0)
            return contains(node.right, item);
        else
            return true;
    }
    public void add(Item item){
        root = add(root.item);
    }

}
*/
