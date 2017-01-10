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

import java.util.Map;
import java.util.Set;

/**
 * Handle windows config file in INI format.
 *
 * @author Kenneth Leung
 */
public interface Win32Conf {

  /**
   */
  public Map<?,?> heading(String heading);

  /**
   */
  public Set<?> headings();

  /**
   */
  public void dbgShow();

  /**
   */
  public String strValue(String heading, String item, String dft);

  /**
   */
  public String strValue(String heading, String item);

  /**
   */
  public long longValue(String heading, String item, long dft);

  /**
   */
  public long longValue(String heading, String item);

  /**
   */
  public int intValue(String heading, String item, int dft);

  /**
   */
  public int intValue(String heading, String item);

  /**
   */
  public boolean boolValue(String heading, String item, boolean dft);

  /**
   */
  public boolean boolValue(String heading, String item);

  /**
   */
  public double doubleValue(String heading, String item, double dft);

  /**
   */
  public double doubleValue(String heading, String item);

}



