/* Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (c) 2013-2016, Kenneth Leung. All rights reserved. */

package czlab.xlib;

import java.util.Map;
import java.util.Set;

/**
 * Handle windows config file in INI format.
 *
 * @author Kenneth Leung
 */
public interface IWin32Conf {

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
  public String strValue(String heading, String prop, String dft);

  /**
   */
  public String strValue(String heading, String prop);

  /**
   */
  public long longValue(String heading, String prop, long dft);

  /**
   */
  public long longValue(String heading, String prop);

  /**
   */
  public int intValue(String heading, String prop, int dft);

  /**
   */
  public int intValue(String heading, String prop);

  /**
   */
  public boolean boolValue(String heading, String prop, boolean dft);

  /**
   */
  public boolean boolValue(String heading, String prop);

  /**
   */
  public double doubleValue(String heading, String prop, double dft);

  /**
   */
  public double doubleValue(String heading, String prop);

}



