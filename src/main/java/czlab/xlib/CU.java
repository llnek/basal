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

import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import static org.slf4j.LoggerFactory.getLogger;
import java.util.GregorianCalendar;
import org.slf4j.Logger;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;


/**
 * @author Kenneth Leung
 */
@SuppressWarnings("unused")
public enum CU {
;

  private static final AtomicInteger _si= new AtomicInteger(0);
  private static final AtomicLong _sn= new AtomicLong(0L);
  public static final Logger TLOG=getLogger(CU.class);

  public static void main(String[] args) {
    try {
      String HTTP_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";
      SimpleDateFormat fmt = new SimpleDateFormat(HTTP_DATE_FORMAT, Locale.US);
      fmt.setTimeZone(TimeZone.getTimeZone("GMT"));
      GregorianCalendar c= new GregorianCalendar();
      Date d1= c.getTime();
      c= new GregorianCalendar(TimeZone.getTimeZone("GMT"));
      Date d2= c.getTime();
      System.out.println("s2 = " + c.getTime());


    } catch (Throwable t) {
      t.printStackTrace();
    }
  }

  /**
   * block and wait on this lock.
   */
  public static void block(Object lock, long waitMillis) {
    try {
      synchronized (lock) {
        if (waitMillis > 0L) {
          lock.wait(waitMillis);
        } else {
          lock.wait();
        }
      }
    }
    catch (Throwable e) {
      TLOG.error("", e);
    }
  }

  /**
   * notify all threads waiting on this lock.
   */
  public static void unblock(Object lock) {
    try {
      synchronized (lock) {
        lock.notifyAll();
      }
    }
    catch (Throwable e) {
      TLOG.error("", e);
    }
  }

  /**
   * cast this to Object.
   */
  public static Object asJObj(Object a) {
    return a;
  }

  /**
   * null string to blank.  safely stringify this object.
   */
  public static String nsb(Object x) {
    return x==null ? "" : x.toString();
  }

  /**
   *
   */
  public static boolean isEmpty(String s) {
    return s==null || s.length() == 0;
  }


  /**
   * shuffle characters in this string.
   */
  public static String shuffle(String s) {
    List<Character> lst = new ArrayList<>();
    char[] cs= s.toCharArray();
    for (int n= 0; n < cs.length; ++n) {
      lst.add(cs[n]);
    }
    Collections.shuffle(lst);
    for (int n= 0; n < lst.size(); ++n) {
      cs[n] = lst.get(n).charValue();
    }
    return new String(cs);
  }

  /**
   * block forever until this thread dies.
   */
  public static void block() {
    try {
      Thread.currentThread().join();
    } catch (Throwable e) {
      TLOG.error("", e);
    }
  }

  /**
   * split a string delimited by a NUL char.
   */
  public static String[] splitNull(String s) {
    return nsb(s).split("\u0000");
  }

  /**
   * load this java class.
   */
  public static Class<?> loadClass(String cz)
    throws ClassNotFoundException {
    return Thread.currentThread().getContextClassLoader().loadClass(cz);
  }

  /**
   * call the default constructor on this java class.
   */
  public static Object dftCtor(String cz)
    throws InstantiationException,
                    IllegalAccessException,
                    IllegalArgumentException,
                    InvocationTargetException,
                    NoSuchMethodException,
                    SecurityException,
                    ClassNotFoundException  {
    return loadClass(cz).getDeclaredConstructor().newInstance();
  }

  /**
   * block and call this function.
   */
  public static Object syncExec(
      Object syncObj,
      CallableWithArgs  r, Object a1, Object... args) throws Exception {

    synchronized(syncObj) {
      return r.run(a1, args);
    }
  }

  /**
   * return next sequence number.
   */
  public static long nextSeqLong() { return _sn.incrementAndGet(); }

  /**
   * return next sequence number.
   */
  public static int nextSeqInt() { return _si.incrementAndGet(); }

}


