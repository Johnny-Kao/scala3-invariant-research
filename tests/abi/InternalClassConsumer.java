import scala.util.hashing.MurmurHash3;

/** Bytecode-exposed but Scala-package-restricted implementation class. */
public class InternalClassConsumer {
  public static void main(String[] args) {
    System.out.println("INTERNAL_ARRAY_HASH=" +
      new MurmurHash3().arrayHash(new int[] {1, 2, 3}));
  }
}
