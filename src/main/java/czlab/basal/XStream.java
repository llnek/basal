/**
 * Copyright © 2013-2020, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.basal;

//import static org.slf4j.LoggerFactory.*;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
//import org.slf4j.Logger;

/**
 * Wrapper on top of a File input stream such that it can
 * delete itself from the file system when garbage collected.
 *
 */
@SuppressWarnings("deprecation")
public class XStream extends InputStream {

  //public static final Logger TLOG= getLogger(XStream.class);
  private transient InputStream _inp = null;
  private boolean _closed = true;
  private boolean _transientFile;
  private File _fn;
  private long pos = 0L;

  /**
   */
  public XStream(File f, boolean isTransient) {
    _transientFile = isTransient;
    _fn= f;
  }

  /**
   */
  public XStream(File f) {
    this(f,false);
  }

  @Override
  public int available() throws IOException {
    pre();
    return _inp.available();
  }

  @Override
  public int read() throws IOException {
    pre();
    int r = _inp.read();
    pos += 1;
    return r;
  }

  @Override
  public int read(byte[] b, int offset, int len) throws IOException {
    if (b == null) { return -1; } else {
      pre();
      int r = _inp.read(b, offset, len);
      pos = (r == -1) ? -1 : pos + r;
      return r;
    }
  }

  @Override
  public int read(byte[] b) throws IOException {
    return (b==null) ? -1 : read(b, 0, b.length);
  }

  @Override
  public long skip(long n) throws IOException {
    if (n < 0L) { return -1L; } else {
      pre();
      long  r= _inp.skip(n);
      if (r > 0L) { pos += r; }
      return r;
    }
  }

  @Override
  public void close() {
    try { if (_inp != null) _inp.close(); }
    catch (Throwable t) {}
    _inp= null;
    _closed= true;
  }

  @Override
  public void mark(int readLimit) {
    if (_inp != null) { _inp.mark(readLimit); }
  }

  @Override
  public void reset() throws IOException {
    if (_inp != null) { _inp.reset(); }
  }

  @Override
  public boolean markSupported() { return true; }

  /**
   */
  public XStream setTransientFlag(boolean dfile) {
    _transientFile = dfile ;
    return this;
  }

  /*
   */
  public void dispose() {
    close();
    if (_transientFile && _fn != null) {
      try { _fn.delete(); } catch (Throwable t) {}
      _fn=null;
    }
  }

  /**
   */
  public String filename() {
    try {
      return (_fn != null) ? _fn.getCanonicalPath() : "" ;
    } catch (IOException e) {
      //if (CU.canLog()) TLOG.error("",e);
      return "";
    }
  }

  @Override
  public String toString() { return filename(); }

  /**
   */
  public long getPosition() { return pos; }

  @Override
  public void finalize() throws Throwable {
    dispose();
  }

  /**
   */
  private void pre() {
    if (_closed) { ready(); }
  }

  /**
   */
  private void ready() {
    close();
    try { _inp= new FileInputStream(_fn); }
    catch (FileNotFoundException e) {
      //if (CU.canLog()) TLOG.error("",e);
    }
    _closed=false;
    pos=0L;
    mark(0);
  }

}



