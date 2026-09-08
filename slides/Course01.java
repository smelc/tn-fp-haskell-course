import java.util.*;
import java.util.stream.*;

class Course01 {

/* Java 7 */
List<Integer> evens(List<Integer> xs) {
  final List<Integer> result = new ArrayList<Integer>(xs.size() / 2);
  for (Integer x : xs) {
    if (x % 2 == 0)
      result.add(x);
  }
  return result;
}

<T> List<T> reverse(List<T> xs) {
  final LinkedList<T> result = new LinkedList<T>();
  for (T x : xs) {
    result.addFirst(x);
  }
  return result;
}

List<Integer> toLengths(List<String> xs) {
  final List<Integer> result = new ArrayList<Integer>(xs.size());
  for (String x : xs) {
    result.add(x.length());
  }
  return result;
}

List<Integer> toLengthsJava8(List<String> xs) {
  return xs.stream().map(String::length).collect(Collectors.toList());
}

/* Requires Java 15 for sealed classes and interfaces */
public sealed interface Version permits Alpha, Beta, SemVer { }

final class Alpha implements Version { }
final class Beta  implements Version { }

/* Requires Java 14 for records */
record SemVer(int x, int y, int z) implements Version { }

}

