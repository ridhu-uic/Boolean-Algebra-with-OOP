package javaCode;/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */
import java.util.List.*;
import java.util.Arrays.*;



public class Pair<T> {
    private final T first;
    private final T second;

    public Pair(T first, T second) {
        this.first = first;
        this.second = second;
    }

    public T first() {
        return first;
    }

    public T second() {
        return second;
    }

    public java.util.List<String> stringList() {
        return java.util.Arrays.asList(String.valueOf(first), String.valueOf(second));
    }

    public static void main(String[] args) {
        Pair <Object> p = new Pair<Object>("TTH", "CS474");
        System.out.println(p.first() + " " + p.second());
        for (String s : p.stringList()) System.out.print(s + " ");
    }
}
