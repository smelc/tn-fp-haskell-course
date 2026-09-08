import java.util.Map;
import java.util.HashMap;

class Course02 {

static Map<Integer, String> buildStudents() {
  var /* no type declared! */ idToStudent = new HashMap<Integer, String>();
  idToStudent.put(1, "Arnold");
  idToStudent.put(2, "Beth");
  return idToStudent;
}

}

