;;; fun.el --- Random funcions set up for fun. -*- lexical-binding: t -*-
;;; Commentary:
;;; Useless but fun Lisp functions.
;;; Code:

(use-package tetris
  :ensure nil
  :custom
  ; 7-bag emacs tetris
  (tetris-allow-repetitions nil))

(use-package gnus
  :ensure nil
  :bind (:map gnus-group-mode-map
         ("r" . gnus-summary-catchup))
  :custom
  (gnus-select-method '(nntp "news.gmane.io")))

;; Jabber (XMPP chat)
(use-package jabber
  :ensure t
  :config
  (defvar w32-notification-id nil)
  (define-jabber-alert w32-notify "Show a message in a toast notification"
                       (lambda (text &optional from)
                         (when (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
                           (when (not (eq w32-notification-id nil))
                             (w32-notification-close w32-notification-id))
                           (setq w32-notification-id
                                 (w32-notification-notify
                                  :title (concat "Jabber"
                                                 (when (not (eq from nil))
                                                   (concat ": " from)))
                                  :body text)))))
  :custom
  (jabber-history-enabled t)
  (jabber-activity-count-in-title t)
  (jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll jabber-message-w32-notify)))

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

(defvar my-cards nil "List of cards made by my-create-card for my-pay-calculator.")
(defun my-create-card (card-name card-balance card-due)
  "Create a card for the payment calculator.
Use a given CARD-NAME, CARD-BALANCE, and the amount CARD-DUE this month."
  (if my-cards
      (add-to-list 'my-cards (list :name card-name :balance card-balance :due card-due))
    (setq my-cards (list (list :name card-name :balance card-balance :due card-due)))))
(defun my-pay-calculator (bank wage wage-count wage-count-next)
  "Calculate the budget for 2 months given a end-of-month card setup in my-cards.
Use the current BANK value, the expected WAGE for both months, and the
number of payments (WAGE-COUNT) for this month and
next (WAGE-COUNT-NEXT) to calculate."
  (let ((cost 0)
        (cost-next 0)
        (card-cost))
    (dolist (card my-cards)
      (setq card-cost (getf card :due)
            cost (+ cost card-cost)
            cost-next (+ cost-next (- (getf card :balance) card-cost))))
    (let* ((wage-this-period (* wage wage-count))
           (wage-next-period (* wage wage-count-next))
           (left-this-month (- (+ bank wage-this-period) cost))
           (left-next-month (- (+ left-this-month wage-next-period) cost-next)))
      (list :due-this-month cost :leftover left-this-month
            :due-next-month cost-next :leftover-next left-next-month))))
;;; fun.el ends here
