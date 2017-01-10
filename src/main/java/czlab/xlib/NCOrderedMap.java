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

import java.util.LinkedHashMap;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A map that has case-ignored string keys.
 *
 * @author Kenneth Leung
 *
 * @param <T>
 */
public class NCOrderedMap<T> extends NCMap<T> {

  private static final long serialVersionUID = -3637175588593032279L;
  //used to keep the order
  private Map<String,T> _order= new LinkedHashMap<>();
  //used to keep original key
  private Map<String,String> _keys= new HashMap<>();


  @Override
  public Set<Map.Entry<String,T>> entrySet() {
    return _order.entrySet();
  }

  @Override
  public T put(String key, T value) {
    _keys.put(key.toLowerCase(), key);
    _order.put(key, value);
    return super.put(key, value);
  }

  @Override
  public T remove(Object k) {
    String key= k.toString();
    String original= _keys.get(key.toLowerCase());
    if (original != null) {
      _keys.remove(key.toLowerCase());
      _order.remove(original);
    }
    return super.remove(key);
  }

  @Override
  public Set<String> keySet() {
    return _order.keySet();
  }

  @Override
  public void clear() {
    _keys.clear();
    _order.clear();
    super.clear();
  }

  @Override
  public Collection<T> values() {
    return _order.values();
  }

  @Override
  public void putAll(Map<? extends String,? extends T> m) {
    for (String s : m.keySet()) {
      put(s, m.get(s));
    }
  }

}



