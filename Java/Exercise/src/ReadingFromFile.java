//package io;
import java.io.*;
public class ReadingFromFile {

    public static void fileRead(String[] args) throws Exception {
        FileReader fr = new FileReader("C:\\DEV\\Java\\Exercise\\src\\Test.java");
        int i;
        while ((i=fr.read()) != -1){
            System.out.print((char) i);
        }
    }
}