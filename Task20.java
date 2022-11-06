import java.math.BigInteger;

public final class Task20 {

    public static void main(String[] args) {
        System.out.println(new Task20().run());
    }
    public String run() {
        String temp = factorial(100).toString();
        int sum = 0;
        for (int i = 0; i < temp.length(); i++)
            sum += temp.charAt(i) - '0';
        return Integer.toString(sum);
    }

    public static BigInteger factorial(int n) {
        BigInteger prod = BigInteger.ONE;
        for (int i = 2; i <= n; i++)
            prod = prod.multiply(BigInteger.valueOf(i));
        return prod;
    }

}