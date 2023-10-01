(in-package #:nyxt-user)

(setq *request-resource-handlers*
      (nconc *request-resource-handlers*
             nx-freestance-handler:*freestance-handlers*))
