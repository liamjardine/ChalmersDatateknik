import java.util.*;
import java.io.*;
import java.nio.file.*;

public class E2 {
    public static void main(String[] args) {


        /////////////////////////// 2. commentRemover
        //String file = ("C:\\DEV\\Java\\Exercise\\src\\Test.java");
        //System.out.println(commentRemover(readAllBytesJava7(file)));
        //fileWrite(commentRemover(readAllBytesJava7(file)).toString(), file);
        //System.out.println(readAllBytesJava7(file));

        /////////////////////////// 3. postFix
        //String postfix = "432*+18-";
        //System.out.println(postFix(postfix));

        /////////////////////////// 3. pushText
        //String line = "it was - the best - of times - - - it was - the - -";
        //System.out.println(pushText(line));

        ArrayList<Integer> arrayList = new ArrayList<>();
        LinkedList<Integer> linkedList = new LinkedList<>();
        Stack<Integer> stack = new Stack<>();

        arrayList.add(123);
        arrayList.add(345);
        arrayList.add(567);

        linkedList.add(1);
        linkedList.add(2);
        linkedList.add(3);
        linkedList.add(4);

        stack.add(1);
        stack.add(2);
        stack.add(3);
        stack.add(4);



        //System.out.println(arrayList.get(0));
        //System.out.println(arrayList.get(1));
        //System.out.println(linkedList.pop());
        //System.out.println(linkedList.pop());
        //System.out.println(stack.pop());
        //System.out.println(stack.pop());

        System.out.println(josephus(3,2));


    }

    /////////////////////////// 2. commentRemover

    public static boolean startComment(char c, char c1) {
        return ((c == '(') && (c1 == '*'));
    }

    public static boolean endComment(char c, char c1) {
        return ((c == '*') && (c1 == ')'));
    }

    public static StringBuilder commentRemover(String args) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < args.length() - 1; i++) {
            char c = args.charAt(i);
            char c1 = args.charAt(i + 1);
            if (startComment(c, c1)) {
                while (!endComment(c, c1)) {
                    i++;
                    c = args.charAt(i);
                    c1 = args.charAt(i + 1);
                }
                i++;
            } else
                sb.append(args.charAt(i));
        }
        sb.append(args.charAt(args.length() - 1));
        return sb;
    }

    private static String readAllBytesJava7(String filePath) {
        String content = "";
        try {
            content = new String(Files.readAllBytes(Paths.get(filePath)));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return content;
    }


    public static void fileWrite(String arg, String fileName) {
        try {
            FileWriter writer = new FileWriter(fileName, false);
            writer.write(arg);
            writer.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    /////////////////////////// Psuedocode from lecture
/*    public static StringBuilder commentRemoverLec(String p){
        StringBuilder st = new StringBuilder();
        String q;
        int depth = 0;
        int k = 0;
        char c;
        while (k < p.length()){
            c = p.charAt(k)+p.charAt(k+1);
            if (c == "(*") {
                depth++;
                k=k+2;
            }
            if (c == "*)"){
                depth--;
                if (depth < 0)
                    return "ERROR";

            }
            if (depth == 0){
                q = q + c;
                k = k+1;
            }
        }
        return sb;
    }*/

    /////////////////////////// 3. postFix

    public static int postFix(String pf) {
        Stack<Integer> st = new Stack<>();

        for (int i = 0; i < pf.length(); i++) {
            char ch = pf.charAt(i);
            if (Character.isDigit(ch))
                st.push(ch - '0');
            else if (st.size()>=2) {
                int value1 = st.pop();
                int value2 = st.pop();

                switch (ch) {
                    case '+':
                        st.push(value2 + value1);
                        break;
                    case '-':
                        st.push(value2 - value1);
                        break;
                    case '*':
                        st.push(value2 * value1);
                        break;
                    case '/':
                        st.push(value2 / value1);
                        break;
                }
            }
            else
                return -1;
        }
        if (st.size()==1)
            return st.pop();
        else
            return -1;
    }

    /////////////////////////// 4. stack

    //it was - the best - of times - - - it was - the - -
    // [it]
    // [was,it]
    // -> was
    // [the,is]
    // [best,the,is]
    // -> best
    // [of,the,is]
    // [times,of,the,is]
    // -> times
    // -> of
    // -> the
    // [it,it]
    // [was,it,it]
    // -> was
    // [the,it,it]
    // -> the
    // -> it
    // [it]
    // 1

    /////////////////////////// 5. queue

    //it was - the best - of times - - - it was - the - -
    // [it]
    // [was,it]
    // -> it
    // [the,was]
    // [best,the,was]
    // -> was
    // [of,best,the]
    // [times,of,best,the]
    // -> the
    // -> best
    // -> of
    // [of]
    // [it,of]
    // [was,it,of]
    // -> of
    // [the,was,it]
    // -> it
    // -> was
    // [the]
    // 1

    /////////////////////////// 4. commentRemover
    /////////////////////////// 1.3.37. Josephus problem

    LinkedList<Integer> list = new LinkedList<>();


    public static Integer josephus(int n, int m){
        Queue<Integer> list = new LinkedList<>();
        for (int i = 0; i < (n-1); i++){
            list.add(i);
        }
        while (list.size()>1){
            for (int j = 0; j < (m-1); j++){
                list.add(list.poll());
            }
            list.remove();
        }
        return list.poll();
    }



}




