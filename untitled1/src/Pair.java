import java.util.Arrays;
import java.util.List;

public class Pair<T1,T2> {
    private final T1 first;

    private T2 f;

    private final T1 second;
    public Pair(T1 first, T1 second) {
        this.first = first;
        this.second = second; }
    public T1 first( ) { return first; }
    public T1 second( ) { return second; }
    public List<String>  stringList( ) {
        return Arrays.asList(String.valueOf(first),String.valueOf(second));
    }
    public static void main(String[ ] args) {



        Pair <Object> p = new Pair<Object>("TTH", "CS474");

                System.out.println(p.first( ) + " " + p.second( ));
        for (String s : p.stringList( )) System.out.print(s + " ");
    }
}