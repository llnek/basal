/**
 * Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.test.xlib;

import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import czlab.xlib.NCMap;
import czlab.xlib.NCOrderedMap;
import junit.framework.JUnit4TestAdapter;

/**
 *
 * @author Kenneth Leung
 *
 */
public class JUnit {

  public static junit.framework.Test suite() {
    return new JUnit4TestAdapter(JUnit.class);
  }

  @BeforeClass
  public static void iniz() throws Exception {
  }

  @AfterClass
  public static void finz() {
  }

  @Before
  public void open() throws Exception {
  }

  @After
  public void close() throws Exception {
  }

  private void testm(Map<String,String> m) throws Exception {
    m.put("AbC", "hello");
    m.put("XYz", "hey");
    m.put("a", "A");
    assertTrue(m.size() == 3);
    assertTrue(m.get("abc") != null);
    assertTrue(m.get("xyz") != null);
    assertTrue(m.get("AbC").equals(m.get("abc")));
    assertTrue(m.get("XYz").equals(m.get("xyz")));
  }

  @Test
  public void testMapOrdered() throws Exception {
    Map<String,String> m= new NCOrderedMap<>();
    testm(m);
    int i=0;
    String[] k= {"AbC", "XYz", "a"};
    for (Map.Entry<String,String> e : m.entrySet()) {
      assertTrue(e.getKey().equals(k[i]));
      ++i;
    }
    String[] vs= {"hello", "hey", "A"};
    i=0;
    for (String v :m.values()) {
      assertTrue(v.equals(vs[i]));
      ++i;
    }
  }

  @Test
  public void testMapNC() throws Exception {
    NCMap<String> m= new NCMap<>();
    testm(m);
  }


}


