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

import static org.slf4j.LoggerFactory.*;
import org.slf4j.Logger;

import java.util.ResourceBundle;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Kenneth Leung
 */
public enum I18N {
;

  private static Map<Object,ResourceBundle> _bs = new HashMap<>();
  private static ResourceBundle _base;
  public static final Logger TLOG= getLogger(I18N.class);

  /**
   */
  public static void setBundle(Object bkey, ResourceBundle b) {
    TLOG.info("setting a resource bundle, bkey = {}", bkey);
    if (b != null) {
      _bs.put(bkey,b);
    }
  }

  /**
   */
  public static ResourceBundle bundle(Object bkey) {
    return _bs.get(bkey);
  }

  /**
   */
  public static void unsetBundle(String bkey) {
    _bs.remove(bkey);
  }

  /**
   */
  public static void setBase(ResourceBundle b) {
    _base=b;
  }

  /**
   */
  public static ResourceBundle base() {
    return _base;
  }

}


