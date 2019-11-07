/**
 * Copyright © 2013-2019, Kenneth Leung. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package czlab.basal;

/**
 * @author Kenneth Leung
 */
public class FailFast extends Exception {

  private static final long serialVersionUID = 1L;

  /**
   */
  public FailFast(String m, Throwable t) {
    super(m, t);
  }

  /**
   */
  public FailFast(Throwable t) {
    super(t);
  }

  /**
   */
  public FailFast(String msg) {
    super(msg);
  }

}



