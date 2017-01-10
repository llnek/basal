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

/**
 * @author Kenneth Leung
 */
public interface Schedulable extends Disposable, Activable {

  /**
   * delay x millis set alarm
   */
  public Object alarm(Interruptable w, Object arg, long delayMillis);

  /**
   * delay x millis before running
   */
  public Object postpone(Runnable w, long delayMillis);

  /**/
  public void purge();

  /**
   */
  public void dequeue(Runnable w);

  /**
   */
  public void run(Runnable w);

  /**
   */
  public void hold(Object pid, Runnable w);

  /**
   */
  public void hold(Runnable w);

  /**
   * take the function out of *hold* state and run it
   */
  public void wakeAndRun(Object pid, Runnable w);

  /**
   */
  public void wakeup(Runnable w);

  /**
   * run this function
   */
  public void reschedule(Runnable w);

}


