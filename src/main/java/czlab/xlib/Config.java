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


import java.util.Date;
import java.util.Set;


/**
 * @author Kenneth Leung
 */
public interface Config {

  /**
   */
  public double doubleValue(String name, double dft);

  /**
   */
  public boolean boolValue(String name, boolean dft);

  /**
   */
  public String strValue(String name, String dft);

  /**
   */
  public long longValue(String name, long dft);

  /**
   */
  public Iterable<?> sequence(String name);

  /**
   */
  public boolean contains(String name);

  /**
   */
  public Date dateValue(String name);

  /**
   */
  public Config child(String name);

  /**
   */
  public int size();

  /**
   */
  public Set<String> keys();

}


