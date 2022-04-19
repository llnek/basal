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
 * Copyright © 2013-2022, Kenneth Leung. All rights reserved. */

package czlab.basal;

//import static org.slf4j.LoggerFactory.getLogger;

import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;
import java.io.CharArrayWriter;
import java.io.File;
import java.nio.charset.Charset;
import java.lang.ref.Cleaner;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.util.Arrays;
//import org.slf4j.Logger;

/**
 * Wrapper structure to abstract a piece of data which can be a file
 * or a memory byte[], String or some object. By default the data
 * is transient and will be purged unless delete is set to false.
 *
 */
@SuppressWarnings("deprecation")
public class XData implements Serializable {

  private static final long serialVersionUID = -8637175588593032279L;
  //public static final Logger TLOG= getLogger(XData.class);
  private String _encoding ="utf-8";
  private boolean _cls=true;
  private Object _data = null;

  /**
   */
  public XData(Object p, boolean delFlag) {
    reset(p,delFlag);
    Cleaner.create().register(this, ()->{
      this.finalise();
    });
  }
  public XData(Object p) {
    this(p,true);
  }
  public XData() {
    this(null);
  }

  /**
   */
  public boolean isDeleteFile() { return _cls; }

  /**
   */
  public String encoding() { return _encoding; }

  /**
   */
  public XData setEncoding(String enc) {
    _encoding=enc;
    return this;
  }

  /**
   * Control the internal file.
   *
   * @param del true to delete
   */
  public XData setDeleteFlag(boolean del) {
    _cls= del;
    return this;
  }

  /*
   */
  public void dispose() {
    if (_cls && _data instanceof File) {
      try { ((File) _data).delete(); }
      catch (Throwable t) {}
      _data=null;
    }
    init();
  }

  /**
   */
  public boolean isFile() {
    return _data instanceof File;
  }

  /**
   */
  public XData reset(Object obj, boolean del) {
    dispose();
    if (obj instanceof ByteArrayOutputStream) {
      _data = ((ByteArrayOutputStream) obj).toByteArray();
    }
    else
    if (obj instanceof CharArrayWriter) {
      _data = ((CharArrayWriter) obj).toCharArray();
    }
    else
    if (obj instanceof File[]) {
      File[] ff= (File[]) obj;
      if (ff.length > 0) { _data = ff[0]; }
    }
    else
    if (obj instanceof XData) {
      XData src= (XData) obj;
      this._encoding = src._encoding;
      this._data = src._data;
      this._cls= src._cls;
      del= src._cls;
      src._data=null;
    }
    else {
      _data=obj;
    }
    setDeleteFlag(del);
    return this;
  }

  /**
   */
  public XData reset(Object obj) {
    return reset(obj, true);
  }

  /**
   */
  public boolean hasContent() { return _data != null; }

  /**
   */
  public Object content() { return _data; }

  /**
   */
  public byte[] getBytes() throws IOException {
    return getBytes(1024 * 1024 * 10); //10meg!
  }

  /**
   */
  public byte[] getBytes(int maxSize) throws IOException {
    byte[] bits=null;

    if (_data instanceof File) {
      File f= (File) _data;
      long n= f.length();
      try (InputStream inp = new FileInputStream(f)) {
        if (n > maxSize) {
          throw new IOException("file too large, " +
                                "size= " +
                                Long.toString(n));
        }
        bits= new byte[(int) n];
        inp.read(bits);
      } catch (IOException e) {
        throw e;
      } catch (Exception e) {
        throw new IOException(e);
      }
    }
    else
    if (_data instanceof String) {
      bits = ((String) _data).getBytes(_encoding);
    }
    else
    if (_data instanceof byte[]) {
      bits = (byte[]) _data;
    }
    else
    if (_data instanceof char[]) {
      _data = toBytes( (char[]) _data );
      bits= (byte[]) _data;
    }
    else
    if (_data != null) {
      throw new IOException("can't getBytes on content");
    }

    return bits;
  }

  /**
   */
  public File fileRef() {
    return _data instanceof File ? ((File) _data) : null;
  }

  /**
   */
  public long size() throws IOException {
    long len=0L;
    if (_data instanceof File) {
      len= ((File) _data).length();
    }
    else
    if (_data instanceof byte[]) {
      len = ((byte[]) _data).length;
    }
    else
    if (_data instanceof String) {
      try {
        len = ((String) _data).getBytes(_encoding).length;
      } catch (Exception e) {
        //if (CU.canLog()) TLOG.error("", e);
      }
    }
    else
    if (_data instanceof char[]) {
      _data = toBytes( (char[]) _data );
      len = ((byte[]) _data).length;
    }
    else
    if (_data != null) {
      throw new IOException("can't getSize on content");
    }

    return len;
  }

  private void finalise(){
    dispose();
  }

  /**
   */
  public String strit() throws IOException {
    return !hasContent()
      ? null
      : (_data instanceof String)
        ? _data.toString()
        : new String(getBytes(), _encoding);
  }

  /**
   */
  public InputStream stream() throws IOException {
    InputStream inp= null;
    if (_data instanceof File) {
      inp = new XStream((File) _data, false);
    }
    else if (hasContent()) {
      inp= new ByteArrayInputStream(getBytes());
    }
    return inp;
  }

  /**
   */
  private void init() {
    _encoding= "utf-8";
    _cls=true;
    _data=null;
  }

  /**
   */
  private byte[] toBytes(char[] cs) {
    ByteBuffer bb = Charset.forName(_encoding).encode(CharBuffer.wrap(cs));
    return Arrays.copyOfRange(bb.array(), bb.position(), bb.limit());
  }

}


