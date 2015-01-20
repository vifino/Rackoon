#lang racket

(require racket/sandbox)
(require racket/format)

(define sb
	(parameterize (
		[sandbox-output 'string]
		[sandbox-error-output 'string]
		[sandbox-eval-limits (list 2 20)]
	) (make-evaluator 'racket/base)
	)
)

(define (evalsb prog)
	(with-handlers ([exn:fail? (lambda (v) (~v v))]) 
		(~v (sb prog))
	)
)

(define (cmd_eval out nick user host chan msg)
	(when (regexp-match? #rx"^\\)->(.*)$" msg)
		(thread (lambda () ( 
			(sb (string-append "(define nick " (format "~v" nick) ")"))
			(sb (string-append "(define chan " (format "~v" chan) ")"))
			(sb (string-append "(define user " (format "~v" user) ")"))
			(sb (string-append "(define host " (format "~v" host) ")"))
			(fprintf out
				(string-append "PRIVMSG " chan " :"
					(evalsb (second (regexp-match #rx"^\\)->(.*)$" msg)));) " ")
				"\r\n")
			)
		)))
	)
)

(provide sb evalsb cmd_eval)
