;;; cpp-styles.el -*- lexical-binding: t; -*-

;; Stroustrup style without namespace indentation
(c-add-style "stroustrup-modified"
             '("stroustrup"
               (c-basic-offset . 2)
               (tab-width . 2)
               (c-offsets-alist
                (innamespace . 0)
                )))
