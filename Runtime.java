import java.util.Scanner;

public class Runtime {

    private static final Scanner in;
    static {
	in = new Scanner(System.in); 
    }
    
    // invokestatic Runtime/printString(Ljava/lang/String;)V
    public static void printString(String s){
	System.out.println(s);
    }
    
    // invokestatic Runtime/printInt(I)V
    public static void printInt(int i){
	System.out.println(i) ;
    }

    // invokestatic Runtime/printDouble(D)V
    public static void printDouble(double d){
	System.out.println(d) ;
    }

    // invokestatic Runtime/readInt()V
    public static int readInt(){
       return in.nextInt();
    }

    // invokestatic Runtime/readDouble()D
    public static double readDouble(){
       return in.nextDouble();

    }

    // invokestatic Runtime/readString()Ljava/lang/String;
    public static String readString(){
       return in.nextLine();
    }

    // invokestatic Runtime/d2Str(D)Ljava/lang/String;
    public static String d2Str(double d){
	return Double.toString(d);
    }

    // invokestatic Runtime/i2Str(I)Ljava/lang/String;
    public static String i2Str(int i){
	return Integer.toString(i);
    }

    // invokestatic Runtime/concatStr(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
    public static String concatStr(String s1,String s2){
	return s1 + s2;
    }
}
