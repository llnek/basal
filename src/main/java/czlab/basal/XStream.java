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

//import static org.slf4j.LoggerFactory.*;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.lang.ref.Cleaner;
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
    Cleaner.create().register(this, ()->{
      this.finalise();
    });
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

  private void finalise(){
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



