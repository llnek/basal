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

import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import static org.slf4j.LoggerFactory.getLogger;
import org.slf4j.Logger;


/**
 * A (thread executor)
 *
 * @author Kenneth Leung
 */
public class TCore extends ThreadPoolExecutor implements RejectedExecutionHandler, Startable, Disposable {

  public static final Logger TLOG = getLogger(TCore.class);
  private boolean _paused;
  private boolean _trace;
  private String _id ="";
  private int _tds = 4;

  /**
   */
  public TCore(String id,
               int tds,
               long keepAliveMillis, boolean trace) {
    super(Math.max(1,tds),
          Math.max(1,tds),
          keepAliveMillis,
          TimeUnit.MILLISECONDS,
          new LinkedBlockingQueue<Runnable>());
    setRejectedExecutionHandler(this);
    setThreadFactory(new TFac(id));
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
    this(id, tds, 5000L, trace);
  }

  /**
   */
  public TCore(String id, int tds) {
    this(id, tds, true);
  }

  @Override
  public void start() {
    _paused=false;
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

  /**
   */
  public void schedule(Runnable r) {
    execute(r);
  }

  @Override
  public void execute(Runnable r) {
    if (! _paused) {
      super.execute(r);
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
      .append(_tds)
      .toString();
  }

}


