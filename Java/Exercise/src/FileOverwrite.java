import java.io.*;

public class FileOverwrite {
    public static void fileWrite(String[] args) throws Exception {
        String st = "Hello World";
        File file = new File("C:\\DEV\\Java\\Exercise\\src\\Test.java");
        if (file.exists()) {
            FileOutputStream fos = new FileOutputStream(file, false);
            fos.write(st.getBytes());
            fos.close();
        }
    }
}