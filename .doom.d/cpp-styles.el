;;; cpp-styles.el -*- lexical-binding: t; -*-

;; Stroustrup style without namespace indentation
(c-add-style "stroustrup-modified"
             '("stroustrup"
               (c-basic-offset . 3)
               (tab-width . 3)
               (c-offsets-alist
                (innamespace . 0)
                )))
