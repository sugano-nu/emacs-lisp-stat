(require 'cl-lib)
(require 'calc)

(defun els-mean (N LIST-OF-DATA)
  "Calculate an arithmetic mean of N th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LIST (mapcar* #'(lambda (x) (nth N x)) LIST-OF-DATA))
  (string-to-number (math-format-number
                     (calcFunc-vmean
                      (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                          LIST))))))

(defun els-varp (N LIST-OF-DATA)
  "Calculate a population variance of N th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LIST (mapcar* #'(lambda (x) (nth N x)) LIST-OF-DATA))
  (string-to-number (math-format-number
                     (calcFunc-vpvar
                      (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                          LIST))))))

(defun els-var (N LIST-OF-DATA)
  "Calculate an unbiased variance of N th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LIST (mapcar* #'(lambda (x) (nth N x)) LIST-OF-DATA))
  (string-to-number (math-format-number
                     (calcFunc-vvar
                      (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                          LIST))))))

(defun els-sd (N LIST-OF-DATA)
  "Calculate an unbiased standard deviation of N th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LIST (mapcar* #'(lambda (x) (nth N x)) LIST-OF-DATA))
  (sqrt (string-to-number (math-format-number
                           (calcFunc-vvar
                            (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                                LIST)))))))

(defun els-cor (N1 N2 LIST-OF-DATA)
  "Calculate a correlation of coefficient of N1 and N2 th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LISTX (mapcar* #'(lambda (x) (nth N1 x)) LIST-OF-DATA))
  (setq LISTY (mapcar* #'(lambda (y) (nth N2 y)) LIST-OF-DATA))
  (string-to-number (math-format-number
                     (calcFunc-vcorr
                      (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                          LISTX))
                      (cons 'vec (mapcar* #'(lambda (Y) (math-read-number Y))
                                          LISTY))))))

(defun els-cov (N1 N2 LIST-OF-DATA)
  "Calculate a covariance of N1 and N2 th column of LIST-OF-DATA.
The first column is 0 th."
  (setq LISTX (mapcar* #'(lambda (x) (nth N1 x)) LIST-OF-DATA))
  (setq LISTY (mapcar* #'(lambda (y) (nth N2 y)) LIST-OF-DATA))
  (string-to-number (math-format-number
                     (calcFunc-vcov
                      (cons 'vec (mapcar* #'(lambda (X) (math-read-number X))
                                          LISTX))
                      (cons 'vec (mapcar* #'(lambda (Y) (math-read-number Y))
                                          LISTY))))))

(defun els-round (VALUE N)
  "Rounds the numeric value 'VALUE' to the specified number 'N' of decimal places."
  (/ (* 1.0 (round (* VALUE (expt 10 N))))
     (expt 10 N)))

(defun els-utpn (x mean dev)
  "The ‘utpn(x,m,s)’ function uses a normal (Gaussian) distribution with
mean ‘m’ and standard deviation ‘s’.  It is the probability that such a
normal-distributed random variable would exceed ‘x’."
  (string-to-number (math-format-number
                     (calcFunc-utpn
                      (math-read-number (number-to-string x))
                      (math-read-number (number-to-string mean))
                      (math-read-number (number-to-string dev))))))

(defun els-ltpn (x mean dev)
  "The ‘utpn(x,m,s)’ function uses a normal (Gaussian) distribution with
mean ‘m’ and standard deviation ‘s’.  It is the probability that such a
normal-distributed random variable would exceed ‘x’."
  (string-to-number (math-format-number
                     (calcFunc-ltpn
                      (math-read-number (number-to-string x))
                      (math-read-number (number-to-string mean))
                      (math-read-number (number-to-string dev))))))

(defun els-utpc (chisq v)
  "The ‘utpc(x,v)’ function uses the chi-square distribution with ‘v’
degrees of freedom.  It is the probability that a model is correct if
its chi-square statistic is ‘x’."
  (string-to-number (math-format-number
                     (calcFunc-utpc
                      (math-read-number (number-to-string chisq))
                      (math-read-number (number-to-string v))
                      ))))

(defun els-ltpc (chisq v)
  "The ‘utpc(x,v)’ function uses the chi-square distribution with ‘v’
degrees of freedom.  It is the probability that a model is correct if
its chi-square statistic is ‘x’."
  (string-to-number (math-format-number
                     (calcFunc-ltpc
                      (math-read-number (number-to-string chisq))
                      (math-read-number (number-to-string v))
                      ))))

(defun els-utpf (f v1 v2)
  "The ‘utpf(F,v1,v2)’ function uses the F distribution, used in various
statistical tests.  The parameters ‘v1’ and ‘v2’ are the degrees of
freedom in the numerator and denominator, respectively, used in
computing the statistic ‘F’."
  (string-to-number (math-format-number
                     (calcFunc-utpf
                      (math-read-number (number-to-string f))
                      (math-read-number (number-to-string v1))
                      (math-read-number (number-to-string v2))
                      ))))

(defun els-ltpf (f v1 v2)
  "The ‘utpf(F,v1,v2)’ function uses the F distribution, used in various
statistical tests.  The parameters ‘v1’ and ‘v2’ are the degrees of
freedom in the numerator and denominator, respectively, used in
computing the statistic ‘F’."
  (string-to-number (math-format-number
                     (calcFunc-ltpf
                      (math-read-number (number-to-string f))
                      (math-read-number (number-to-string v1))
                      (math-read-number (number-to-string v2))
                      ))))

(defun els-utpp (n x)
  "The ‘utpp(n,x)’ function uses a Poisson distribution with mean ‘x’.
It is the probability that ‘n’ or more such Poisson random events will
occur."
  (string-to-number (math-format-number
                     (calcFunc-utpp
                      (math-read-number (number-to-string n))
                      (math-read-number (number-to-string x))
                      ))))

(defun els-ltpp (n x)
  "The ‘utpp(n,x)’ function uses a Poisson distribution with mean ‘x’.
It is the probability that ‘n’ or more such Poisson random events will
occur."
  (string-to-number (math-format-number
                     (calcFunc-ltpp
                      (math-read-number (number-to-string n))
                      (math-read-number (number-to-string x))
                      ))))

(defun els-utpt (tt v)
  "The ‘utpt(t,v)’ function uses the Student’s “t” distribution with ‘v’
degrees of freedom.  It is the probability that a t-distributed random
variable will be greater than ‘t’.  (Note: This computes the
distribution function ‘A(t|v)’ where ‘A(0|v) = 1’ and ‘A(inf|v) -> 0’.
The ‘UTPT’ operation on the HP-48 uses a different definition which
returns half of Calc’s value: ‘UTPT(t,v) = .5*utpt(t,v)’.)"
  (string-to-number (math-format-number
                     (calcFunc-utpt
                      (math-read-number (number-to-string tt))
                      (math-read-number (number-to-string v))
                      ))))

(defun els-ltpt (tt v)
  "The ‘utpt(t,v)’ function uses the Student’s “t” distribution with ‘v’
degrees of freedom.  It is the probability that a t-distributed random
variable will be greater than ‘t’.  (Note: This computes the
distribution function ‘A(t|v)’ where ‘A(0|v) = 1’ and ‘A(inf|v) -> 0’.
The ‘UTPT’ operation on the HP-48 uses a different definition which
returns half of Calc’s value: ‘UTPT(t,v) = .5*utpt(t,v)’.)"
  (string-to-number (math-format-number
                     (calcFunc-ltpt
                      (math-read-number (number-to-string tt))
                      (math-read-number (number-to-string v))
                      ))))
