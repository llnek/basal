/**
 * Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.xlib;

import java.util.Comparator;
import java.util.TreeMap;

/**
 * A map that has case-ignored string keys.
 *
 * @author Kenneth Leung
 *
 * @param <T>
 */
public class NCMap<T> extends TreeMap<String, T> implements java.io.Serializable {

  private static final long serialVersionUID = -3637175588593032279L;

  /**
   */
  public NCMap() {
    super(new NoCase<String>());
  }

  /**
   */
  private static class NoCase<T> implements Comparator<T> {
    public int compare(T o1, T o2) {
      String s1 = o1 == null ? "" : o1.toString();
      String s2 = o2 == null ? "" : o2.toString();
      return s1.toUpperCase().compareTo(s2.toUpperCase());
    }
  }

}


