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
    TLOG.debug("setting a resource bundle, bkey = {}", bkey);
    if (bkey != null && b != null) {
      _bs.put(bkey,b);
    }
  }

  /**
   */
  public static ResourceBundle bundle(Object bkey) {
    return bkey == null ? null : _bs.get(bkey);
  }

  /**
   */
  public static void unsetBundle(Object bkey) {
    if (bkey != null) { _bs.remove(bkey); }
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


