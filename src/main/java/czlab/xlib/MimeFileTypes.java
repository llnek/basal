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


import javax.activation.MimetypesFileTypeMap;
import java.io.UnsupportedEncodingException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Map.Entry;
import java.util.Properties;


/**
 * @author Kenneth Leung
 */
public class MimeFileTypes {

  /**
   * load mime file extensions/types
   */
  public static MimetypesFileTypeMap makeMimeFileTypes(Properties props) throws IOException {
    StringBuilder sum = new StringBuilder();
    for (Entry<Object,Object> en : props.entrySet()) {
      sum.append(en.getValue().toString().trim()  +
          "  " +
          en.getKey().toString().trim()  + "\n");
    }
    try {
      return new MimetypesFileTypeMap(new ByteArrayInputStream( sum.toString().getBytes("utf-8")) );
    } catch (UnsupportedEncodingException e) {
      throw new IOException("Failed to parse mime.properties.");
    }
  }

}


