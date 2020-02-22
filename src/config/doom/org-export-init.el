;;; -*- lexical-binding: t; -*-

;; TODO Grab more of the org specific setup
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(require 'org) 
(require 'ox)
(require 'cl)  
(setq org-export-async-debug nil)
