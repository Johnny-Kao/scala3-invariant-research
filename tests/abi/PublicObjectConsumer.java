import scala.util.hashing.MurmurHash3$;

/** Public Scala companion-object call compiled against the pre-fix RC. */
public class PublicObjectConsumer {
  public static void main(String[] args) {
    System.out.println("PUBLIC_ARRAY_HASH=" +
      MurmurHash3$.MODULE$.arrayHash(new int[] {1, 2, 3}));
  }
}
