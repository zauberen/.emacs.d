;;; fun.el --- Random funcions set up for fun.
;;; Commentary:
;;; Useless but fun Lisp functions.
;;; Code:
(defun my-interest-calculator (payment interest-rate loan-amt &optional suppress-disp)
  "Calculates interest for a loan given a payment.
PAYMENT The monthly payment.
INTEREST-RATE The yearly interest rate.
LOAN-AMT The starting loan amount.
SUPPRESS-DISP display statistics if nil."
  (let ((monthly-rate (/ interest-rate 12))
        (min-pay (* (/ interest-rate 12) loan-amt))
        (this-payment payment)
        (months-paid 0)
        (interest-paid 0)
        (interest-accrued 0)
        (total-paid 0))
    (if (> min-pay payment)
        (progn
          (princ "Payment must be greater than: ")
          (princ min-pay))
      (while (> loan-amt 0)
        (setf months-paid (+ months-paid 1)
              this-payment (if (> loan-amt payment) payment loan-amt)
              total-paid (+ total-paid this-payment)
              interest-accrued (* (- loan-amt this-payment) monthly-rate)
              interest-paid (+ interest-paid interest-accrued)
              loan-amt (+ (- loan-amt this-payment) interest-accrued))))
    (when (and (eq suppress-disp nil) (> months-paid 0))
      (princ "Max interest/mo: ")
      (princ min-pay)
      (princ " Months paid: ")
      (princ months-paid)
      (when (> months-paid 12)
        (princ "(")
        (princ (/ months-paid 12))
        (princ " years and ")
        (princ (% months-paid 12))
        (princ " months)"))
      (princ " Interest paid: ")
      (princ interest-paid)
      (princ " Total paid: ")
      (princ total-paid))
    (list :months-paid months-paid :interest-paid interest-paid :total-paid total-paid :payment payment)))

(defun my-payment-comparison (payment1 payment2 interest-rate loan-amt)
  "Compares 2 payment plans for a loan.
PAYMENT1 The monthly payment option 1.
PAYMENT2 The monthly payment option 2.
INTEREST-RATE The yearly interest rate.
LOAN-AMT The starting loan amount."
  (let ((pplan1 (my-interest-calculator payment1 interest-rate loan-amt t))
        (pplan2 (my-interest-calculator payment2 interest-rate loan-amt t))
        (better-plan (list 1))
        (worse-plan (list 2)))
    (if (> (getf pplan1 :total-paid) (getf pplan2 :total-paid))
      (setf better-plan pplan2
            worse-plan pplan1)
      (setf better-plan pplan1
            worse-plan pplan2))
    (list :savings (- (getf worse-plan :total-paid) (getf better-plan :total-paid))
          :savings-in-months (/ (- (getf worse-plan :total-paid) (getf better-plan :total-paid)) (getf worse-plan :payment))
          :optimal-plan better-plan)))
;;; fun.el ends here
