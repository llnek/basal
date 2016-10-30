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


