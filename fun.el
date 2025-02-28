;;; fun.el --- Random funcions set up for fun.
;;; Commentary:
;;; Useless but fun Lisp functions.
;;; Code:
(defun my-interest-calculator (payment interest-rate loan-amt)
  "Calculates interest for a loan given a payment.
PAYMENT The monthly payment.
INTEREST-RATE The yearly interest rate.
LOAN-AMT The starting loan amount."
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
    (when (> months-paid 0)
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
    total-paid))
;;; fun.el ends here
