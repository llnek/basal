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

import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.slf4j.LoggerFactory.getLogger;
import org.slf4j.Logger;


/**
 * A (thread executor)
 *
 * @author Kenneth Leung
 */
public class TCore extends ThreadPoolExecutor implements RejectedExecutionHandler, Startable, Disposable {

  public static final Logger TLOG = getLogger(TCore.class);
  private AtomicInteger _seq= new AtomicInteger(0);
  private boolean _paused;
  private boolean _trace;
  private String _id ="";

  /**
   */
  public TCore(final String id,
               int tds,
               long keepAliveMillis, boolean trace) {
    super(Math.max(1,tds),
          Math.max(1,tds),
          keepAliveMillis,
          TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue<Runnable>());
    setThreadFactory(new ThreadFactory() {
      public Thread newThread(Runnable r) {
        ClassLoader cl = Thread.currentThread().getContextClassLoader();
        Thread t = new Thread(r);
        t.setName(id + "#" + _seq.incrementAndGet());
        t.setContextClassLoader(cl);
        //t.setPriority(Thread.NORM_PRIORITY);
        //t.setDaemon(false);
        return t;
      }
    });
    setRejectedExecutionHandler(this);
    _trace=trace;
    _id=id;
    _paused=true;
    if (trace) {
      TLOG.debug("TCore#{} created with threads = {}",
          id , "" + getCorePoolSize());
    }
  }

  /**
   */
  public TCore(String id, int tds, boolean trace) {
    this(id, tds, 60000L, trace);
  }

  /**
   */
  public TCore(String id, int tds) {
    this(id, tds, true);
  }

  /**
   */
  public TCore(String id) {
    this(id, Runtime.getRuntime().availableProcessors() * 2);
  }

  @Override
  public Object restart(Object arg) {
    return start(arg);
  }

  @Override
  public Object start(Object arg) {
    _paused=false;
    return this;
  }

  @Override
  public void stop() {
    _paused=true;
  }

  @Override
  public void dispose() {
    stop();
    shutdown();
    if (_trace) {
      TLOG.debug("TCore#{} disposed and shut down", _id);
    }
  }

  @Override
  public void execute(Runnable r) {
    if (! _paused) {
      super.execute(r);
    } else {
      TLOG.warn("Ignoring the runnable, core is not running");
    }
  }

  @Override
  public void rejectedExecution(Runnable r, ThreadPoolExecutor x) {
    //TODO: deal with too much work for the core...
    TLOG.error("TCore#{} rejecting work!", _id);
  }

  @Override
  public String toString() {
    return new StringBuilder("TCore#")
      .append(_id)
      .append(" with threads = ")
      .append(getCorePoolSize())
      .toString();
  }

}


