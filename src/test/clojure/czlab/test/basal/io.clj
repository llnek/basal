;; Copyright ©  2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.test.basal.io

  (:require [clojure.java.io :as io]
            [clojure.string :as cs]
            [clojure.test :as ct]
            [czlab.basal.io :as i]
            [czlab.basal.util :as u]
            [czlab.basal.core
             :refer [ensure?? ensure-thrown??] :as c])

  (:import [java.net
            URL]
           [java.io
            File
            InputStream
            ByteArrayOutputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
(def ^:private ^File TMP_DIR i/*tempfile-repo*)
(def ^:private ^File TMP_FP
  (io/file TMP_DIR (str (u/jid<>) ".txt")))
(eval '(do (spit TMP_FP "heeloo" :encoding "utf-8"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c/deftest test-io

  (ensure?? "x->chars,x->bytes,x->str"
            (and (= "heeloo" (i/x->str (i/x->chars (i/x->bytes
                                                     (.toCharArray "heeloo")))))
                 (= "ab"
                    (c/wo* [baos (i/baos<>)
                            inp (io/input-stream (i/x->bytes "abcde"))]
                      (i/copy inp baos 2)
                      (i/x->str baos "utf-8")))
                 (= "abcde"
                    (c/wo* [baos (i/baos<> 100)
                            inp (io/input-stream (i/x->bytes "abcde"))]
                      (i/copy inp baos 100)
                      (i/x->str baos)))
                 (= "abcde"
                    (c/wo* [baos (ByteArrayOutputStream.)
                            inp (io/input-stream (i/x->bytes "abcde"))]
                      (i/copy inp baos)
                      (i/x->str baos)))
                 (= "abcde"
                    (c/wo* [inp (io/input-stream (i/x->bytes "abcde"))]
                      (i/x->str (i/x->bytes inp))))
                 (= "hello"
                    (i/x->str (i/x->bytes (.toCharArray "hello"))))
                 (= "hello"
                    (String. (i/x->chars (.getBytes "hello" "utf-8"))))))

  (ensure?? "readable-bytes"
            (= 5 (c/wo* [inp (io/input-stream (i/x->bytes "hello"))]
                   (i/readable-bytes inp))))

  (ensure?? "klose" (do (i/klose nil)
                        (i/klose (i/baos<>)) true))

  (ensure?? "write-number"
            (and (= 4 (alength (i/write-number Integer/MAX_VALUE)))
                 (= 8 (alength (i/write-number Long/MAX_VALUE)))
                 (= Long/MAX_VALUE
                    (i/read-number (i/write-number Long/MAX_VALUE) Long))
                 (= Integer/MAX_VALUE
                    (i/read-number (i/write-number Integer/MAX_VALUE) Integer))
                 (= Integer/MAX_VALUE
                    (i/read-number (i/write-number Integer/MAX_VALUE) Integer))
                 (= Long/MAX_VALUE
                    (i/read-number (i/write-number Long/MAX_VALUE) Long))))

  (ensure?? "input-stream??"
            (and (nil? (i/input-stream?? 333))
                 (let [[c? i]
                       (i/input-stream?? "aaa")]
                   (if c? (i/klose i))
                   (c/is? InputStream i))))

  (ensure?? "bytes->hex" (c/is? u/CSCZ (i/bytes->hex (.getBytes "abc"))))

  (ensure?? "hexify" (= "616263" (i/hexify (.getBytes "abc"))))

  (ensure?? "gzip,gunzip"
            (= "hello"
               (i/x->str (i/gunzip (i/gzip (i/x->bytes "hello"))))))

  (ensure?? "gzb64->bytes,bytes->gzb64"
            (= "helloworld"
               (i/x->str (i/gzb64->bytes
                           (i/bytes->gzb64 (i/x->bytes "helloworld"))))))

  (ensure?? "slurp-bytes"
            (c/wo* [inp (i/res->stream "czlab/basal/etc/mime.properties")]
              (binding [i/*membuf-limit* (* 2 1024)]
                (let [z (.available inp)
                      x (i/slurp-bytes inp)
                      n (alength (i/x->bytes x))]
                  (i/fdelete x)
                  (= z n)))))

  (ensure?? "slurp-chars"
            (c/wo* [inp (i/res->stream "czlab/basal/etc/mime.properties")]
              (binding [i/*membuf-limit* (* 2 1024)]
                (let [z (.available inp)
                      x (i/slurp-chars inp)
                      n (alength (i/x->bytes x))]
                  (i/fdelete x)
                  (= z n)))))

  (ensure?? "res->stream"
            (c/wo* [s (i/res->stream "czlab/basal/etc/sample.ini")]
              (c/is? InputStream s)))

  (ensure?? "res->url"
            (c/is? URL (i/res->url "czlab/basal/etc/sample.ini")))

  (ensure?? "res->str"
            (let [s (i/res->str "czlab/basal/etc/sample.ini")]
              (cs/includes? s "Microsoft")))

  (ensure?? "res->bytes"
            (let [b (i/res->bytes "czlab/basal/etc/sample.ini")
                  s (i/x->str b)]
              (cs/includes? s "Microsoft")))

  (ensure?? "reset-stream!"
            (c/wo* [s (i/res->stream "czlab/basal/etc/sample.ini")]
              (.mark s (int 10))
              (let [c (.read s)
                    _ (i/reset-stream! s)
                    c2 (.read s)]
                (= c c2))))

  (ensure?? "temp-file" (let [f (i/temp-file)
                              _ (spit f "a")
                              e? (.exists f)] (i/fdelete f) e?))

  (ensure?? "open-temp-file"
            (let [[f os] (i/open-temp-file)
                  o? (some? os)] (i/klose os) (i/fdelete f) o?))

  (ensure?? "x->file"
            (= "hello"
               (let [inp (io/input-stream (i/x->bytes "hello"))
                     f (i/x->file inp)
                     s (slurp f)] (i/klose inp) (i/fdelete f) s)))

  ;(ensure?? "reset-source!" (i/reset-source! ))

  (ensure?? "work-dir-path" (string? (i/work-dir-path)))

  ;(ensure?? "unzip->dir" (i/unzip->dir ))

  ;(ensure?? "chunk-read-stream" (i/chunk-read-stream))

  (ensure?? "fmt->edn" (string? (i/fmt->edn
                                  {:a 1 :b {:c {:e "hello"} :d 4}})))

  (ensure?? "read-edn"
            (let [s (i/fmt->edn
                      {:a 1 :b {:c {:e "hello"} :d 4}})
                  t (i/temp-file)
                  _ (spit t s)
                  e (i/read-edn t)]
              (i/fdelete t)
              (and (string? s)
                   (= "hello" (get-in e [:b :c :e])))))

  (ensure?? "fmt->json" (string? (i/fmt->json
                                   {:a 1 :b {:c {:e "hello"} :d 4}})))

  (ensure?? "read-json"
            (let [s (i/fmt->json
                      {:a 1 :b {:c {:e "hello"} :d 4}})
                  t (i/temp-file)
                  _ (spit t s)
                  e (i/read-json t "utf-8" keyword)]
              (i/fdelete t)
              (and (string? s)
                   (= "hello" (get-in e [:b :c :e])))))

  (ensure?? "temp-file"
            (= "hello"
               (let [f (i/temp-file)]
                 (i/spit-utf8 f "hello")
                 (try (i/slurp-utf8 f)
                      (finally (i/fdelete f))))))

  (ensure?? "parent-path,parent-file"
            (let [f (i/temp-file)
                  p (.getCanonicalPath f)
                  pp (i/parent-path p)
                  dd (io/file pp)
                  d (i/parent-file f)
                  _ (i/spit-utf8 f "hello")
                  d? (i/dir-read? d)
                  dw? (i/dir-read-write? d)
                  x? (i/can-exec? d)
                  ok? (i/file-ok? f)
                  r? (i/file-read? f)
                  f? (i/file-read-write? f)]
              (i/fdelete f)
              (and ;;(= d dd)
                   d? dw? x? ok? r? f?)))

  (ensure?? "touch!"
            (let [f (i/temp-file)
                  _ (i/spit-utf8 f "hello")
                  t (.lastModified f)
                  p (.getCanonicalPath f)
                  f2 (str p ".t")]
              (u/pause 1000)
              (i/touch! f)
              (i/touch! f2)
              (let [rc (and (> (.lastModified f) t)
                            (i/file-ok? (io/file f2)))]
                (i/fdelete f)
                (i/fdelete f2)
                rc)))

  (ensure?? "change-content"
            (= "helloworld"
               (let [f (i/temp-file)]
                 (i/spit-utf8 f "hello")
                 (try (i/change-content f #(str % "world"))
                      (finally (i/fdelete f))))))

  (ensure?? "replace-file!"
            (= "helloworld"
               (let [f (i/temp-file)]
                 (i/spit-utf8 f "hello")
                 (i/replace-file! f #(str % "world"))
                 (try (i/slurp-utf8 f)
                      (finally (i/fdelete f))))))

  (ensure?? "spit-utf8, slurp-utf8"
            (= "hello"
               (let [f (i/temp-file)]
                 (i/spit-utf8 f "hello")
                 (try (i/slurp-utf8 f)
                      (finally (i/fdelete f))))))

  (ensure?? "slurp-bytes"
            (= 5 (let [f (i/temp-file)]
                   (i/spit-utf8 f "hello")
                   (try (alength (i/x->bytes
                                   (i/slurp-bytes f)))
                        (finally (i/fdelete f))))))

  (ensure?? "spit-utf8"
            (= "hello"
               (let [f (i/temp-file)]
                 (i/spit-utf8 f "hello")
                 (try (i/x->str f)
                      (finally (i/fdelete f))))))

  (ensure?? "mkdirs"
            (let [n (u/jid<>)
                  d (io/file i/*tempfile-repo* n)
                  _ (i/mkdirs d)
                  e? (i/file-ok? d)]
              (i/fdelete d) e?))

  (ensure?? "list-dirs,list-files,list-any-files"
            (let [n0 (u/jid<>)
                  n1 (u/jid<>)
                  n2 (u/jid<>)
                  f1 (str n1 ".txt")
                  f2 (str n2 ".txt")
                  root (io/file i/*tempfile-repo* n0)
                  d1 (io/file root n1)
                  d2 (io/file root n2)
                  _ (i/mkdirs root)
                  _ (i/mkdirs d1)
                  _ (i/mkdirs d2)
                  _ (i/spit-utf8 (io/file root f1) "a")
                  _ (i/spit-utf8 (io/file root f2) "a")
                  _ (i/spit-utf8 (io/file d1 f1) "a")
                  _ (i/spit-utf8 (io/file d2 f2) "a")
                  dz (count (i/list-dirs root))
                  fz (count (i/list-files root ".txt"))
                  tz (count (i/list-any-files root ".txt"))]
              (i/fdelete (io/file d1 f1))
              (i/fdelete (io/file d2 f2))
              (i/fdelete (io/file root f1))
              (i/fdelete (io/file root f2))
              (i/fdelete (io/file d1))
              (i/fdelete (io/file d2))
              (i/fdelete root)
              (and (= 2 dz) (= 2 fz) (= 4 tz))))

  (ensure?? "grep-folder-paths,grep-file-paths"
            (let [n0 (u/jid<>) n1 (u/jid<>) n2 (u/jid<>)
                  n3 (u/jid<>) n4 (u/jid<>) n5 (u/jid<>)
                  root (io/file i/*tempfile-repo* n0)
                  _ (i/mkdirs root)
                  d1 (doto (io/file root n1) (i/mkdirs))
                  d2 (doto (io/file root n2) (i/mkdirs))
                  d3 (doto (io/file root n3) (i/mkdirs))
                  d4 (doto (io/file d2 n4) (i/mkdirs))
                  d5 (doto (io/file d4 n5) (i/mkdirs))
                  _ (i/spit-utf8 (io/file d1 "d1.txt") "a")
                  _ (i/spit-utf8 (io/file d4 "d4a.txt") "a")
                  _ (i/spit-utf8 (io/file d4 "d4b.txt") "a")
                  _ (i/spit-utf8 (io/file d5 "d5.txt") "a")
                  ds (i/grep-folder-paths root ".txt")
                  fs (i/grep-file-paths root ".txt")]
              (i/fdelete (io/file d1 "d1.txt"))
              (i/fdelete (io/file d4 "d4a.txt"))
              (i/fdelete (io/file d4 "d4b.txt"))
              (i/fdelete (io/file d5 "d5.txt"))
              (i/fdelete d5)
              (i/fdelete d4)
              (i/fdelete d1)
              (i/fdelete d2)
              (i/fdelete d3)
              (i/fdelete root)
              (and (= 3 (count ds))
                   (= 4 (count fs)))))

  (ensure?? "basename"
            (let [n (u/jid<>)
                  f (io/file i/*tempfile-repo* (str n ".txt"))
                  _ (i/spit-utf8 f "hello")
                  b (i/basename f)]
              (i/fdelete f)
              (= n b)))

  (ensure?? "test-end" (= 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ct/deftest ^:test-io basal-test-io
  (ct/is (let [[ok? r]
               (c/runtest test-io "test-io")] (println r) ok?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


