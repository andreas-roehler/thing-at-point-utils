;;; thingatpt-count-ratio.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2017 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; Keywords: convenience

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'thingatpt-utils-core)

;;; count/ratio ar-atpt-rest-list ar-atpt-classes start

(defun ar-count-alnum-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of alnum in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of alnum in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-greateranglednested-atpt (&optional beg end) 
"Relation of alnum in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of alpha in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of alpha in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-greateranglednested-atpt (&optional beg end) 
"Relation of alpha in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of ascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of ascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-greateranglednested-atpt (&optional beg end) 
"Relation of ascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of blank in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of blank in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-greateranglednested-atpt (&optional beg end) 
"Relation of blank in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of cntrl in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of cntrl in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-greateranglednested-atpt (&optional beg end) 
"Relation of cntrl in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of digit in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of digit in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-greateranglednested-atpt (&optional beg end) 
"Relation of digit in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of graph in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of graph in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-greateranglednested-atpt (&optional beg end) 
"Relation of graph in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of lower in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of lower in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-greateranglednested-atpt (&optional beg end) 
"Relation of lower in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of nonascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of nonascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-greateranglednested-atpt (&optional beg end) 
"Relation of nonascii in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of print in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of print in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-greateranglednested-atpt (&optional beg end) 
"Relation of print in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of punct in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of punct in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-greateranglednested-atpt (&optional beg end) 
"Relation of punct in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of space in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of space in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-greateranglednested-atpt (&optional beg end) 
"Relation of space in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of upper in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of upper in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-greateranglednested-atpt (&optional beg end) 
"Relation of upper in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of alnum in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of alnum in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-lesseranglednested-atpt (&optional beg end) 
"Relation of alnum in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of alpha in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of alpha in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-lesseranglednested-atpt (&optional beg end) 
"Relation of alpha in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of ascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of ascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-lesseranglednested-atpt (&optional beg end) 
"Relation of ascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of blank in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of blank in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-lesseranglednested-atpt (&optional beg end) 
"Relation of blank in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of cntrl in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of cntrl in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-lesseranglednested-atpt (&optional beg end) 
"Relation of cntrl in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of digit in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of digit in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-lesseranglednested-atpt (&optional beg end) 
"Relation of digit in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of graph in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of graph in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-lesseranglednested-atpt (&optional beg end) 
"Relation of graph in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of lower in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of lower in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-lesseranglednested-atpt (&optional beg end) 
"Relation of lower in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of nonascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of nonascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-lesseranglednested-atpt (&optional beg end) 
"Relation of nonascii in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of print in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of print in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-lesseranglednested-atpt (&optional beg end) 
"Relation of print in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of punct in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of punct in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-lesseranglednested-atpt (&optional beg end) 
"Relation of punct in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of space in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of space in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-lesseranglednested-atpt (&optional beg end) 
"Relation of space in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of upper in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of upper in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-lesseranglednested-atpt (&optional beg end) 
"Relation of upper in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-buffer-atpt (&optional beg end) 
"Counts matches of alnum in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-buffer-until-point (&optional beg end) 
"Counts matches of alnum in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-buffer-atpt (&optional beg end) 
"Relation of alnum in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-buffer-atpt (&optional beg end) 
"Counts matches of alpha in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-buffer-until-point (&optional beg end) 
"Counts matches of alpha in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-buffer-atpt (&optional beg end) 
"Relation of alpha in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-buffer-atpt (&optional beg end) 
"Counts matches of ascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-buffer-until-point (&optional beg end) 
"Counts matches of ascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-buffer-atpt (&optional beg end) 
"Relation of ascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-buffer-atpt (&optional beg end) 
"Counts matches of blank in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-buffer-until-point (&optional beg end) 
"Counts matches of blank in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-buffer-atpt (&optional beg end) 
"Relation of blank in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-buffer-atpt (&optional beg end) 
"Counts matches of cntrl in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-buffer-until-point (&optional beg end) 
"Counts matches of cntrl in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-buffer-atpt (&optional beg end) 
"Relation of cntrl in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-buffer-atpt (&optional beg end) 
"Counts matches of digit in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-buffer-until-point (&optional beg end) 
"Counts matches of digit in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-buffer-atpt (&optional beg end) 
"Relation of digit in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-buffer-atpt (&optional beg end) 
"Counts matches of graph in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-buffer-until-point (&optional beg end) 
"Counts matches of graph in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-buffer-atpt (&optional beg end) 
"Relation of graph in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-buffer-atpt (&optional beg end) 
"Counts matches of lower in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-buffer-until-point (&optional beg end) 
"Counts matches of lower in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-buffer-atpt (&optional beg end) 
"Relation of lower in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-buffer-atpt (&optional beg end) 
"Counts matches of nonascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-buffer-until-point (&optional beg end) 
"Counts matches of nonascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-buffer-atpt (&optional beg end) 
"Relation of nonascii in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-buffer-atpt (&optional beg end) 
"Counts matches of print in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-buffer-until-point (&optional beg end) 
"Counts matches of print in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-buffer-atpt (&optional beg end) 
"Relation of print in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-buffer-atpt (&optional beg end) 
"Counts matches of punct in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-buffer-until-point (&optional beg end) 
"Counts matches of punct in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-buffer-atpt (&optional beg end) 
"Relation of punct in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-buffer-atpt (&optional beg end) 
"Counts matches of space in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-buffer-until-point (&optional beg end) 
"Counts matches of space in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-buffer-atpt (&optional beg end) 
"Relation of space in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-buffer-atpt (&optional beg end) 
"Counts matches of upper in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-buffer-until-point (&optional beg end) 
"Counts matches of upper in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-buffer-atpt (&optional beg end) 
"Relation of upper in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-comment-atpt (&optional beg end) 
"Counts matches of alnum in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-comment-until-point (&optional beg end) 
"Counts matches of alnum in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-comment-atpt (&optional beg end) 
"Relation of alnum in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-comment-atpt (&optional beg end) 
"Counts matches of alpha in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-comment-until-point (&optional beg end) 
"Counts matches of alpha in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-comment-atpt (&optional beg end) 
"Relation of alpha in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-comment-atpt (&optional beg end) 
"Counts matches of ascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-comment-until-point (&optional beg end) 
"Counts matches of ascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-comment-atpt (&optional beg end) 
"Relation of ascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-comment-atpt (&optional beg end) 
"Counts matches of blank in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-comment-until-point (&optional beg end) 
"Counts matches of blank in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-comment-atpt (&optional beg end) 
"Relation of blank in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-comment-atpt (&optional beg end) 
"Counts matches of cntrl in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-comment-until-point (&optional beg end) 
"Counts matches of cntrl in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-comment-atpt (&optional beg end) 
"Relation of cntrl in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-comment-atpt (&optional beg end) 
"Counts matches of digit in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-comment-until-point (&optional beg end) 
"Counts matches of digit in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-comment-atpt (&optional beg end) 
"Relation of digit in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-comment-atpt (&optional beg end) 
"Counts matches of graph in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-comment-until-point (&optional beg end) 
"Counts matches of graph in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-comment-atpt (&optional beg end) 
"Relation of graph in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-comment-atpt (&optional beg end) 
"Counts matches of lower in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-comment-until-point (&optional beg end) 
"Counts matches of lower in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-comment-atpt (&optional beg end) 
"Relation of lower in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-comment-atpt (&optional beg end) 
"Counts matches of nonascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-comment-until-point (&optional beg end) 
"Counts matches of nonascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-comment-atpt (&optional beg end) 
"Relation of nonascii in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-comment-atpt (&optional beg end) 
"Counts matches of print in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-comment-until-point (&optional beg end) 
"Counts matches of print in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-comment-atpt (&optional beg end) 
"Relation of print in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-comment-atpt (&optional beg end) 
"Counts matches of punct in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-comment-until-point (&optional beg end) 
"Counts matches of punct in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-comment-atpt (&optional beg end) 
"Relation of punct in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-comment-atpt (&optional beg end) 
"Counts matches of space in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-comment-until-point (&optional beg end) 
"Counts matches of space in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-comment-atpt (&optional beg end) 
"Relation of space in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-comment-atpt (&optional beg end) 
"Counts matches of upper in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'comment beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-comment-until-point (&optional beg end) 
"Counts matches of upper in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'comment beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-comment-atpt (&optional beg end) 
"Relation of upper in comment if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'comment beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-csv-atpt (&optional beg end) 
"Counts matches of alnum in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-csv-until-point (&optional beg end) 
"Counts matches of alnum in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-csv-atpt (&optional beg end) 
"Relation of alnum in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-csv-atpt (&optional beg end) 
"Counts matches of alpha in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-csv-until-point (&optional beg end) 
"Counts matches of alpha in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-csv-atpt (&optional beg end) 
"Relation of alpha in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-csv-atpt (&optional beg end) 
"Counts matches of ascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-csv-until-point (&optional beg end) 
"Counts matches of ascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-csv-atpt (&optional beg end) 
"Relation of ascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-csv-atpt (&optional beg end) 
"Counts matches of blank in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-csv-until-point (&optional beg end) 
"Counts matches of blank in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-csv-atpt (&optional beg end) 
"Relation of blank in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-csv-atpt (&optional beg end) 
"Counts matches of cntrl in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-csv-until-point (&optional beg end) 
"Counts matches of cntrl in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-csv-atpt (&optional beg end) 
"Relation of cntrl in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-csv-atpt (&optional beg end) 
"Counts matches of digit in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-csv-until-point (&optional beg end) 
"Counts matches of digit in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-csv-atpt (&optional beg end) 
"Relation of digit in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-csv-atpt (&optional beg end) 
"Counts matches of graph in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-csv-until-point (&optional beg end) 
"Counts matches of graph in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-csv-atpt (&optional beg end) 
"Relation of graph in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-csv-atpt (&optional beg end) 
"Counts matches of lower in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-csv-until-point (&optional beg end) 
"Counts matches of lower in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-csv-atpt (&optional beg end) 
"Relation of lower in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-csv-atpt (&optional beg end) 
"Counts matches of nonascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-csv-until-point (&optional beg end) 
"Counts matches of nonascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-csv-atpt (&optional beg end) 
"Relation of nonascii in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-csv-atpt (&optional beg end) 
"Counts matches of print in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-csv-until-point (&optional beg end) 
"Counts matches of print in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-csv-atpt (&optional beg end) 
"Relation of print in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-csv-atpt (&optional beg end) 
"Counts matches of punct in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-csv-until-point (&optional beg end) 
"Counts matches of punct in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-csv-atpt (&optional beg end) 
"Relation of punct in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-csv-atpt (&optional beg end) 
"Counts matches of space in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-csv-until-point (&optional beg end) 
"Counts matches of space in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-csv-atpt (&optional beg end) 
"Relation of space in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-csv-atpt (&optional beg end) 
"Counts matches of upper in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-csv-until-point (&optional beg end) 
"Counts matches of upper in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-csv-atpt (&optional beg end) 
"Relation of upper in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-date-atpt (&optional beg end) 
"Counts matches of alnum in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-date-until-point (&optional beg end) 
"Counts matches of alnum in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-date-atpt (&optional beg end) 
"Relation of alnum in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'date beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-date-atpt (&optional beg end) 
"Counts matches of alpha in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-date-until-point (&optional beg end) 
"Counts matches of alpha in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-date-atpt (&optional beg end) 
"Relation of alpha in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'date beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-date-atpt (&optional beg end) 
"Counts matches of ascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-date-until-point (&optional beg end) 
"Counts matches of ascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-date-atpt (&optional beg end) 
"Relation of ascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'date beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-date-atpt (&optional beg end) 
"Counts matches of blank in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-date-until-point (&optional beg end) 
"Counts matches of blank in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-date-atpt (&optional beg end) 
"Relation of blank in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'date beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-date-atpt (&optional beg end) 
"Counts matches of cntrl in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-date-until-point (&optional beg end) 
"Counts matches of cntrl in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-date-atpt (&optional beg end) 
"Relation of cntrl in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'date beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-date-atpt (&optional beg end) 
"Counts matches of digit in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-date-until-point (&optional beg end) 
"Counts matches of digit in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-date-atpt (&optional beg end) 
"Relation of digit in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'date beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-date-atpt (&optional beg end) 
"Counts matches of graph in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-date-until-point (&optional beg end) 
"Counts matches of graph in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-date-atpt (&optional beg end) 
"Relation of graph in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'date beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-date-atpt (&optional beg end) 
"Counts matches of lower in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-date-until-point (&optional beg end) 
"Counts matches of lower in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-date-atpt (&optional beg end) 
"Relation of lower in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'date beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-date-atpt (&optional beg end) 
"Counts matches of nonascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-date-until-point (&optional beg end) 
"Counts matches of nonascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-date-atpt (&optional beg end) 
"Relation of nonascii in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'date beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-date-atpt (&optional beg end) 
"Counts matches of print in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-date-until-point (&optional beg end) 
"Counts matches of print in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-date-atpt (&optional beg end) 
"Relation of print in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'date beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-date-atpt (&optional beg end) 
"Counts matches of punct in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-date-until-point (&optional beg end) 
"Counts matches of punct in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-date-atpt (&optional beg end) 
"Relation of punct in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'date beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-date-atpt (&optional beg end) 
"Counts matches of space in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-date-until-point (&optional beg end) 
"Counts matches of space in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-date-atpt (&optional beg end) 
"Relation of space in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'date beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-date-atpt (&optional beg end) 
"Counts matches of upper in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'date beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-date-until-point (&optional beg end) 
"Counts matches of upper in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'date beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-date-atpt (&optional beg end) 
"Relation of upper in date if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'date beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-email-atpt (&optional beg end) 
"Counts matches of alnum in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-email-until-point (&optional beg end) 
"Counts matches of alnum in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-email-atpt (&optional beg end) 
"Relation of alnum in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'email beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-email-atpt (&optional beg end) 
"Counts matches of alpha in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-email-until-point (&optional beg end) 
"Counts matches of alpha in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-email-atpt (&optional beg end) 
"Relation of alpha in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'email beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-email-atpt (&optional beg end) 
"Counts matches of ascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-email-until-point (&optional beg end) 
"Counts matches of ascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-email-atpt (&optional beg end) 
"Relation of ascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'email beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-email-atpt (&optional beg end) 
"Counts matches of blank in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-email-until-point (&optional beg end) 
"Counts matches of blank in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-email-atpt (&optional beg end) 
"Relation of blank in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'email beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-email-atpt (&optional beg end) 
"Counts matches of cntrl in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-email-until-point (&optional beg end) 
"Counts matches of cntrl in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-email-atpt (&optional beg end) 
"Relation of cntrl in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'email beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-email-atpt (&optional beg end) 
"Counts matches of digit in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-email-until-point (&optional beg end) 
"Counts matches of digit in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-email-atpt (&optional beg end) 
"Relation of digit in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'email beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-email-atpt (&optional beg end) 
"Counts matches of graph in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-email-until-point (&optional beg end) 
"Counts matches of graph in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-email-atpt (&optional beg end) 
"Relation of graph in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'email beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-email-atpt (&optional beg end) 
"Counts matches of lower in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-email-until-point (&optional beg end) 
"Counts matches of lower in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-email-atpt (&optional beg end) 
"Relation of lower in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'email beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-email-atpt (&optional beg end) 
"Counts matches of nonascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-email-until-point (&optional beg end) 
"Counts matches of nonascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-email-atpt (&optional beg end) 
"Relation of nonascii in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'email beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-email-atpt (&optional beg end) 
"Counts matches of print in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-email-until-point (&optional beg end) 
"Counts matches of print in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-email-atpt (&optional beg end) 
"Relation of print in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'email beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-email-atpt (&optional beg end) 
"Counts matches of punct in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-email-until-point (&optional beg end) 
"Counts matches of punct in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-email-atpt (&optional beg end) 
"Relation of punct in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'email beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-email-atpt (&optional beg end) 
"Counts matches of space in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-email-until-point (&optional beg end) 
"Counts matches of space in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-email-atpt (&optional beg end) 
"Relation of space in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'email beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-email-atpt (&optional beg end) 
"Counts matches of upper in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'email beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-email-until-point (&optional beg end) 
"Counts matches of upper in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'email beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-email-atpt (&optional beg end) 
"Relation of upper in email if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'email beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-filename-atpt (&optional beg end) 
"Counts matches of alnum in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-filename-until-point (&optional beg end) 
"Counts matches of alnum in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-filename-atpt (&optional beg end) 
"Relation of alnum in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-filename-atpt (&optional beg end) 
"Counts matches of alpha in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-filename-until-point (&optional beg end) 
"Counts matches of alpha in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-filename-atpt (&optional beg end) 
"Relation of alpha in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-filename-atpt (&optional beg end) 
"Counts matches of ascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-filename-until-point (&optional beg end) 
"Counts matches of ascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-filename-atpt (&optional beg end) 
"Relation of ascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-filename-atpt (&optional beg end) 
"Counts matches of blank in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-filename-until-point (&optional beg end) 
"Counts matches of blank in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-filename-atpt (&optional beg end) 
"Relation of blank in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-filename-atpt (&optional beg end) 
"Counts matches of cntrl in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-filename-until-point (&optional beg end) 
"Counts matches of cntrl in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-filename-atpt (&optional beg end) 
"Relation of cntrl in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-filename-atpt (&optional beg end) 
"Counts matches of digit in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-filename-until-point (&optional beg end) 
"Counts matches of digit in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-filename-atpt (&optional beg end) 
"Relation of digit in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-filename-atpt (&optional beg end) 
"Counts matches of graph in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-filename-until-point (&optional beg end) 
"Counts matches of graph in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-filename-atpt (&optional beg end) 
"Relation of graph in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-filename-atpt (&optional beg end) 
"Counts matches of lower in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-filename-until-point (&optional beg end) 
"Counts matches of lower in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-filename-atpt (&optional beg end) 
"Relation of lower in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-filename-atpt (&optional beg end) 
"Counts matches of nonascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-filename-until-point (&optional beg end) 
"Counts matches of nonascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-filename-atpt (&optional beg end) 
"Relation of nonascii in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-filename-atpt (&optional beg end) 
"Counts matches of print in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-filename-until-point (&optional beg end) 
"Counts matches of print in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-filename-atpt (&optional beg end) 
"Relation of print in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-filename-atpt (&optional beg end) 
"Counts matches of punct in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-filename-until-point (&optional beg end) 
"Counts matches of punct in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-filename-atpt (&optional beg end) 
"Relation of punct in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-filename-atpt (&optional beg end) 
"Counts matches of space in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-filename-until-point (&optional beg end) 
"Counts matches of space in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-filename-atpt (&optional beg end) 
"Relation of space in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-filename-atpt (&optional beg end) 
"Counts matches of upper in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filename beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-filename-until-point (&optional beg end) 
"Counts matches of upper in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filename beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-filename-atpt (&optional beg end) 
"Relation of upper in filename if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filename beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of alnum in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of alnum in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of alnum in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of alpha in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of alpha in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of alpha in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of ascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of ascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of ascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of blank in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of blank in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of blank in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of cntrl in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of cntrl in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of cntrl in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of digit in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of digit in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of digit in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of graph in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of graph in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of graph in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of lower in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of lower in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of lower in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of nonascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of nonascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of nonascii in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of print in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of print in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of print in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of punct in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of punct in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of punct in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of space in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of space in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of space in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-filenamenondirectory-atpt (&optional beg end) 
"Counts matches of upper in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filenamenondirectory beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-filenamenondirectory-until-point (&optional beg end) 
"Counts matches of upper in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filenamenondirectory beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-filenamenondirectory-atpt (&optional beg end) 
"Relation of upper in filenamenondirectory if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'filenamenondirectory beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-function-atpt (&optional beg end) 
"Counts matches of alnum in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-function-until-point (&optional beg end) 
"Counts matches of alnum in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-function-atpt (&optional beg end) 
"Relation of alnum in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'function beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-function-atpt (&optional beg end) 
"Counts matches of alpha in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-function-until-point (&optional beg end) 
"Counts matches of alpha in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-function-atpt (&optional beg end) 
"Relation of alpha in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'function beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-function-atpt (&optional beg end) 
"Counts matches of ascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-function-until-point (&optional beg end) 
"Counts matches of ascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-function-atpt (&optional beg end) 
"Relation of ascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'function beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-function-atpt (&optional beg end) 
"Counts matches of blank in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-function-until-point (&optional beg end) 
"Counts matches of blank in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-function-atpt (&optional beg end) 
"Relation of blank in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'function beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-function-atpt (&optional beg end) 
"Counts matches of cntrl in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-function-until-point (&optional beg end) 
"Counts matches of cntrl in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-function-atpt (&optional beg end) 
"Relation of cntrl in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'function beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-function-atpt (&optional beg end) 
"Counts matches of digit in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-function-until-point (&optional beg end) 
"Counts matches of digit in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-function-atpt (&optional beg end) 
"Relation of digit in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'function beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-function-atpt (&optional beg end) 
"Counts matches of graph in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-function-until-point (&optional beg end) 
"Counts matches of graph in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-function-atpt (&optional beg end) 
"Relation of graph in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'function beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-function-atpt (&optional beg end) 
"Counts matches of lower in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-function-until-point (&optional beg end) 
"Counts matches of lower in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-function-atpt (&optional beg end) 
"Relation of lower in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'function beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-function-atpt (&optional beg end) 
"Counts matches of nonascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-function-until-point (&optional beg end) 
"Counts matches of nonascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-function-atpt (&optional beg end) 
"Relation of nonascii in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'function beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-function-atpt (&optional beg end) 
"Counts matches of print in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-function-until-point (&optional beg end) 
"Counts matches of print in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-function-atpt (&optional beg end) 
"Relation of print in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'function beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-function-atpt (&optional beg end) 
"Counts matches of punct in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-function-until-point (&optional beg end) 
"Counts matches of punct in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-function-atpt (&optional beg end) 
"Relation of punct in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'function beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-function-atpt (&optional beg end) 
"Counts matches of space in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-function-until-point (&optional beg end) 
"Counts matches of space in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-function-atpt (&optional beg end) 
"Relation of space in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'function beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-function-atpt (&optional beg end) 
"Counts matches of upper in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'function beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-function-until-point (&optional beg end) 
"Counts matches of upper in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'function beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-function-atpt (&optional beg end) 
"Relation of upper in function if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'function beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-ip-atpt (&optional beg end) 
"Counts matches of alnum in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-ip-until-point (&optional beg end) 
"Counts matches of alnum in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-ip-atpt (&optional beg end) 
"Relation of alnum in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-ip-atpt (&optional beg end) 
"Counts matches of alpha in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-ip-until-point (&optional beg end) 
"Counts matches of alpha in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-ip-atpt (&optional beg end) 
"Relation of alpha in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-ip-atpt (&optional beg end) 
"Counts matches of ascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-ip-until-point (&optional beg end) 
"Counts matches of ascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-ip-atpt (&optional beg end) 
"Relation of ascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-ip-atpt (&optional beg end) 
"Counts matches of blank in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-ip-until-point (&optional beg end) 
"Counts matches of blank in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-ip-atpt (&optional beg end) 
"Relation of blank in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-ip-atpt (&optional beg end) 
"Counts matches of cntrl in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-ip-until-point (&optional beg end) 
"Counts matches of cntrl in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-ip-atpt (&optional beg end) 
"Relation of cntrl in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-ip-atpt (&optional beg end) 
"Counts matches of digit in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-ip-until-point (&optional beg end) 
"Counts matches of digit in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-ip-atpt (&optional beg end) 
"Relation of digit in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-ip-atpt (&optional beg end) 
"Counts matches of graph in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-ip-until-point (&optional beg end) 
"Counts matches of graph in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-ip-atpt (&optional beg end) 
"Relation of graph in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-ip-atpt (&optional beg end) 
"Counts matches of lower in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-ip-until-point (&optional beg end) 
"Counts matches of lower in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-ip-atpt (&optional beg end) 
"Relation of lower in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-ip-atpt (&optional beg end) 
"Counts matches of nonascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-ip-until-point (&optional beg end) 
"Counts matches of nonascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-ip-atpt (&optional beg end) 
"Relation of nonascii in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-ip-atpt (&optional beg end) 
"Counts matches of print in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-ip-until-point (&optional beg end) 
"Counts matches of print in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-ip-atpt (&optional beg end) 
"Relation of print in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-ip-atpt (&optional beg end) 
"Counts matches of punct in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-ip-until-point (&optional beg end) 
"Counts matches of punct in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-ip-atpt (&optional beg end) 
"Relation of punct in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-ip-atpt (&optional beg end) 
"Counts matches of space in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-ip-until-point (&optional beg end) 
"Counts matches of space in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-ip-atpt (&optional beg end) 
"Relation of space in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-ip-atpt (&optional beg end) 
"Counts matches of upper in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'ip beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-ip-until-point (&optional beg end) 
"Counts matches of upper in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'ip beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-ip-atpt (&optional beg end) 
"Relation of upper in ip if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'ip beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-line-atpt (&optional beg end) 
"Counts matches of alnum in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-line-until-point (&optional beg end) 
"Counts matches of alnum in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-line-atpt (&optional beg end) 
"Relation of alnum in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'line beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-line-atpt (&optional beg end) 
"Counts matches of alpha in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-line-until-point (&optional beg end) 
"Counts matches of alpha in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-line-atpt (&optional beg end) 
"Relation of alpha in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'line beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-line-atpt (&optional beg end) 
"Counts matches of ascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-line-until-point (&optional beg end) 
"Counts matches of ascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-line-atpt (&optional beg end) 
"Relation of ascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'line beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-line-atpt (&optional beg end) 
"Counts matches of blank in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-line-until-point (&optional beg end) 
"Counts matches of blank in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-line-atpt (&optional beg end) 
"Relation of blank in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'line beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-line-atpt (&optional beg end) 
"Counts matches of cntrl in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-line-until-point (&optional beg end) 
"Counts matches of cntrl in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-line-atpt (&optional beg end) 
"Relation of cntrl in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'line beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-line-atpt (&optional beg end) 
"Counts matches of digit in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-line-until-point (&optional beg end) 
"Counts matches of digit in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-line-atpt (&optional beg end) 
"Relation of digit in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'line beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-line-atpt (&optional beg end) 
"Counts matches of graph in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-line-until-point (&optional beg end) 
"Counts matches of graph in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-line-atpt (&optional beg end) 
"Relation of graph in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'line beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-line-atpt (&optional beg end) 
"Counts matches of lower in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-line-until-point (&optional beg end) 
"Counts matches of lower in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-line-atpt (&optional beg end) 
"Relation of lower in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'line beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-line-atpt (&optional beg end) 
"Counts matches of nonascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-line-until-point (&optional beg end) 
"Counts matches of nonascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-line-atpt (&optional beg end) 
"Relation of nonascii in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'line beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-line-atpt (&optional beg end) 
"Counts matches of print in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-line-until-point (&optional beg end) 
"Counts matches of print in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-line-atpt (&optional beg end) 
"Relation of print in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'line beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-line-atpt (&optional beg end) 
"Counts matches of punct in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-line-until-point (&optional beg end) 
"Counts matches of punct in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-line-atpt (&optional beg end) 
"Relation of punct in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'line beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-line-atpt (&optional beg end) 
"Counts matches of space in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-line-until-point (&optional beg end) 
"Counts matches of space in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-line-atpt (&optional beg end) 
"Relation of space in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'line beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-line-atpt (&optional beg end) 
"Counts matches of upper in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-line-until-point (&optional beg end) 
"Counts matches of upper in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-line-atpt (&optional beg end) 
"Relation of upper in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'line beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-list-atpt (&optional beg end) 
"Counts matches of alnum in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-list-until-point (&optional beg end) 
"Counts matches of alnum in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-list-atpt (&optional beg end) 
"Relation of alnum in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'list beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-list-atpt (&optional beg end) 
"Counts matches of alpha in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-list-until-point (&optional beg end) 
"Counts matches of alpha in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-list-atpt (&optional beg end) 
"Relation of alpha in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'list beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-list-atpt (&optional beg end) 
"Counts matches of ascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-list-until-point (&optional beg end) 
"Counts matches of ascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-list-atpt (&optional beg end) 
"Relation of ascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'list beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-list-atpt (&optional beg end) 
"Counts matches of blank in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-list-until-point (&optional beg end) 
"Counts matches of blank in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-list-atpt (&optional beg end) 
"Relation of blank in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'list beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-list-atpt (&optional beg end) 
"Counts matches of cntrl in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-list-until-point (&optional beg end) 
"Counts matches of cntrl in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-list-atpt (&optional beg end) 
"Relation of cntrl in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'list beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-list-atpt (&optional beg end) 
"Counts matches of digit in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-list-until-point (&optional beg end) 
"Counts matches of digit in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-list-atpt (&optional beg end) 
"Relation of digit in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'list beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-list-atpt (&optional beg end) 
"Counts matches of graph in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-list-until-point (&optional beg end) 
"Counts matches of graph in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-list-atpt (&optional beg end) 
"Relation of graph in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'list beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-list-atpt (&optional beg end) 
"Counts matches of lower in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-list-until-point (&optional beg end) 
"Counts matches of lower in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-list-atpt (&optional beg end) 
"Relation of lower in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'list beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-list-atpt (&optional beg end) 
"Counts matches of nonascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-list-until-point (&optional beg end) 
"Counts matches of nonascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-list-atpt (&optional beg end) 
"Relation of nonascii in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'list beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-list-atpt (&optional beg end) 
"Counts matches of print in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-list-until-point (&optional beg end) 
"Counts matches of print in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-list-atpt (&optional beg end) 
"Relation of print in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'list beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-list-atpt (&optional beg end) 
"Counts matches of punct in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-list-until-point (&optional beg end) 
"Counts matches of punct in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-list-atpt (&optional beg end) 
"Relation of punct in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'list beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-list-atpt (&optional beg end) 
"Counts matches of space in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-list-until-point (&optional beg end) 
"Counts matches of space in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-list-atpt (&optional beg end) 
"Relation of space in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'list beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-list-atpt (&optional beg end) 
"Counts matches of upper in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'list beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-list-until-point (&optional beg end) 
"Counts matches of upper in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'list beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-list-atpt (&optional beg end) 
"Relation of upper in list if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'list beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-name-atpt (&optional beg end) 
"Counts matches of alnum in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-name-until-point (&optional beg end) 
"Counts matches of alnum in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-name-atpt (&optional beg end) 
"Relation of alnum in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'name beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-name-atpt (&optional beg end) 
"Counts matches of alpha in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-name-until-point (&optional beg end) 
"Counts matches of alpha in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-name-atpt (&optional beg end) 
"Relation of alpha in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'name beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-name-atpt (&optional beg end) 
"Counts matches of ascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-name-until-point (&optional beg end) 
"Counts matches of ascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-name-atpt (&optional beg end) 
"Relation of ascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'name beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-name-atpt (&optional beg end) 
"Counts matches of blank in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-name-until-point (&optional beg end) 
"Counts matches of blank in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-name-atpt (&optional beg end) 
"Relation of blank in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'name beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-name-atpt (&optional beg end) 
"Counts matches of cntrl in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-name-until-point (&optional beg end) 
"Counts matches of cntrl in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-name-atpt (&optional beg end) 
"Relation of cntrl in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'name beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-name-atpt (&optional beg end) 
"Counts matches of digit in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-name-until-point (&optional beg end) 
"Counts matches of digit in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-name-atpt (&optional beg end) 
"Relation of digit in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'name beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-name-atpt (&optional beg end) 
"Counts matches of graph in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-name-until-point (&optional beg end) 
"Counts matches of graph in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-name-atpt (&optional beg end) 
"Relation of graph in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'name beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-name-atpt (&optional beg end) 
"Counts matches of lower in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-name-until-point (&optional beg end) 
"Counts matches of lower in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-name-atpt (&optional beg end) 
"Relation of lower in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'name beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-name-atpt (&optional beg end) 
"Counts matches of nonascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-name-until-point (&optional beg end) 
"Counts matches of nonascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-name-atpt (&optional beg end) 
"Relation of nonascii in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'name beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-name-atpt (&optional beg end) 
"Counts matches of print in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-name-until-point (&optional beg end) 
"Counts matches of print in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-name-atpt (&optional beg end) 
"Relation of print in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'name beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-name-atpt (&optional beg end) 
"Counts matches of punct in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-name-until-point (&optional beg end) 
"Counts matches of punct in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-name-atpt (&optional beg end) 
"Relation of punct in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'name beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-name-atpt (&optional beg end) 
"Counts matches of space in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-name-until-point (&optional beg end) 
"Counts matches of space in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-name-atpt (&optional beg end) 
"Relation of space in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'name beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-name-atpt (&optional beg end) 
"Counts matches of upper in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'name beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-name-until-point (&optional beg end) 
"Counts matches of upper in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'name beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-name-atpt (&optional beg end) 
"Relation of upper in name if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'name beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-page-atpt (&optional beg end) 
"Counts matches of alnum in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-page-until-point (&optional beg end) 
"Counts matches of alnum in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-page-atpt (&optional beg end) 
"Relation of alnum in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'page beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-page-atpt (&optional beg end) 
"Counts matches of alpha in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-page-until-point (&optional beg end) 
"Counts matches of alpha in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-page-atpt (&optional beg end) 
"Relation of alpha in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'page beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-page-atpt (&optional beg end) 
"Counts matches of ascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-page-until-point (&optional beg end) 
"Counts matches of ascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-page-atpt (&optional beg end) 
"Relation of ascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'page beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-page-atpt (&optional beg end) 
"Counts matches of blank in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-page-until-point (&optional beg end) 
"Counts matches of blank in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-page-atpt (&optional beg end) 
"Relation of blank in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'page beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-page-atpt (&optional beg end) 
"Counts matches of cntrl in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-page-until-point (&optional beg end) 
"Counts matches of cntrl in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-page-atpt (&optional beg end) 
"Relation of cntrl in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'page beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-page-atpt (&optional beg end) 
"Counts matches of digit in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-page-until-point (&optional beg end) 
"Counts matches of digit in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-page-atpt (&optional beg end) 
"Relation of digit in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'page beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-page-atpt (&optional beg end) 
"Counts matches of graph in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-page-until-point (&optional beg end) 
"Counts matches of graph in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-page-atpt (&optional beg end) 
"Relation of graph in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'page beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-page-atpt (&optional beg end) 
"Counts matches of lower in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-page-until-point (&optional beg end) 
"Counts matches of lower in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-page-atpt (&optional beg end) 
"Relation of lower in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'page beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-page-atpt (&optional beg end) 
"Counts matches of nonascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-page-until-point (&optional beg end) 
"Counts matches of nonascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-page-atpt (&optional beg end) 
"Relation of nonascii in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'page beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-page-atpt (&optional beg end) 
"Counts matches of print in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-page-until-point (&optional beg end) 
"Counts matches of print in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-page-atpt (&optional beg end) 
"Relation of print in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'page beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-page-atpt (&optional beg end) 
"Counts matches of punct in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-page-until-point (&optional beg end) 
"Counts matches of punct in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-page-atpt (&optional beg end) 
"Relation of punct in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'page beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-page-atpt (&optional beg end) 
"Counts matches of space in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-page-until-point (&optional beg end) 
"Counts matches of space in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-page-atpt (&optional beg end) 
"Relation of space in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'page beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-page-atpt (&optional beg end) 
"Counts matches of upper in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'page beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-page-until-point (&optional beg end) 
"Counts matches of upper in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'page beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-page-atpt (&optional beg end) 
"Relation of upper in page if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'page beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-paragraph-atpt (&optional beg end) 
"Counts matches of alnum in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-paragraph-until-point (&optional beg end) 
"Counts matches of alnum in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-paragraph-atpt (&optional beg end) 
"Relation of alnum in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-paragraph-atpt (&optional beg end) 
"Counts matches of alpha in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-paragraph-until-point (&optional beg end) 
"Counts matches of alpha in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-paragraph-atpt (&optional beg end) 
"Relation of alpha in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-paragraph-atpt (&optional beg end) 
"Counts matches of ascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-paragraph-until-point (&optional beg end) 
"Counts matches of ascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-paragraph-atpt (&optional beg end) 
"Relation of ascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-paragraph-atpt (&optional beg end) 
"Counts matches of blank in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-paragraph-until-point (&optional beg end) 
"Counts matches of blank in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-paragraph-atpt (&optional beg end) 
"Relation of blank in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-paragraph-atpt (&optional beg end) 
"Counts matches of cntrl in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-paragraph-until-point (&optional beg end) 
"Counts matches of cntrl in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-paragraph-atpt (&optional beg end) 
"Relation of cntrl in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-paragraph-atpt (&optional beg end) 
"Counts matches of digit in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-paragraph-until-point (&optional beg end) 
"Counts matches of digit in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-paragraph-atpt (&optional beg end) 
"Relation of digit in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-paragraph-atpt (&optional beg end) 
"Counts matches of graph in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-paragraph-until-point (&optional beg end) 
"Counts matches of graph in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-paragraph-atpt (&optional beg end) 
"Relation of graph in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-paragraph-atpt (&optional beg end) 
"Counts matches of lower in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-paragraph-until-point (&optional beg end) 
"Counts matches of lower in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-paragraph-atpt (&optional beg end) 
"Relation of lower in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-paragraph-atpt (&optional beg end) 
"Counts matches of nonascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-paragraph-until-point (&optional beg end) 
"Counts matches of nonascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-paragraph-atpt (&optional beg end) 
"Relation of nonascii in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-paragraph-atpt (&optional beg end) 
"Counts matches of print in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-paragraph-until-point (&optional beg end) 
"Counts matches of print in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-paragraph-atpt (&optional beg end) 
"Relation of print in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-paragraph-atpt (&optional beg end) 
"Counts matches of punct in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-paragraph-until-point (&optional beg end) 
"Counts matches of punct in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-paragraph-atpt (&optional beg end) 
"Relation of punct in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-paragraph-atpt (&optional beg end) 
"Counts matches of space in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-paragraph-until-point (&optional beg end) 
"Counts matches of space in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-paragraph-atpt (&optional beg end) 
"Relation of space in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-paragraph-atpt (&optional beg end) 
"Counts matches of upper in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-paragraph-until-point (&optional beg end) 
"Counts matches of upper in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-paragraph-atpt (&optional beg end) 
"Relation of upper in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-phone-atpt (&optional beg end) 
"Counts matches of alnum in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-phone-until-point (&optional beg end) 
"Counts matches of alnum in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-phone-atpt (&optional beg end) 
"Relation of alnum in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-phone-atpt (&optional beg end) 
"Counts matches of alpha in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-phone-until-point (&optional beg end) 
"Counts matches of alpha in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-phone-atpt (&optional beg end) 
"Relation of alpha in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-phone-atpt (&optional beg end) 
"Counts matches of ascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-phone-until-point (&optional beg end) 
"Counts matches of ascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-phone-atpt (&optional beg end) 
"Relation of ascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-phone-atpt (&optional beg end) 
"Counts matches of blank in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-phone-until-point (&optional beg end) 
"Counts matches of blank in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-phone-atpt (&optional beg end) 
"Relation of blank in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-phone-atpt (&optional beg end) 
"Counts matches of cntrl in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-phone-until-point (&optional beg end) 
"Counts matches of cntrl in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-phone-atpt (&optional beg end) 
"Relation of cntrl in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-phone-atpt (&optional beg end) 
"Counts matches of digit in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-phone-until-point (&optional beg end) 
"Counts matches of digit in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-phone-atpt (&optional beg end) 
"Relation of digit in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-phone-atpt (&optional beg end) 
"Counts matches of graph in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-phone-until-point (&optional beg end) 
"Counts matches of graph in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-phone-atpt (&optional beg end) 
"Relation of graph in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-phone-atpt (&optional beg end) 
"Counts matches of lower in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-phone-until-point (&optional beg end) 
"Counts matches of lower in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-phone-atpt (&optional beg end) 
"Relation of lower in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-phone-atpt (&optional beg end) 
"Counts matches of nonascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-phone-until-point (&optional beg end) 
"Counts matches of nonascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-phone-atpt (&optional beg end) 
"Relation of nonascii in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-phone-atpt (&optional beg end) 
"Counts matches of print in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-phone-until-point (&optional beg end) 
"Counts matches of print in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-phone-atpt (&optional beg end) 
"Relation of print in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-phone-atpt (&optional beg end) 
"Counts matches of punct in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-phone-until-point (&optional beg end) 
"Counts matches of punct in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-phone-atpt (&optional beg end) 
"Relation of punct in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-phone-atpt (&optional beg end) 
"Counts matches of space in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-phone-until-point (&optional beg end) 
"Counts matches of space in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-phone-atpt (&optional beg end) 
"Relation of space in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-phone-atpt (&optional beg end) 
"Counts matches of upper in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'phone beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-phone-until-point (&optional beg end) 
"Counts matches of upper in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'phone beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-phone-atpt (&optional beg end) 
"Relation of upper in phone if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'phone beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-region-atpt (&optional beg end) 
"Counts matches of alnum in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-region-until-point (&optional beg end) 
"Counts matches of alnum in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-region-atpt (&optional beg end) 
"Relation of alnum in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'region beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-region-atpt (&optional beg end) 
"Counts matches of alpha in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-region-until-point (&optional beg end) 
"Counts matches of alpha in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-region-atpt (&optional beg end) 
"Relation of alpha in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'region beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-region-atpt (&optional beg end) 
"Counts matches of ascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-region-until-point (&optional beg end) 
"Counts matches of ascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-region-atpt (&optional beg end) 
"Relation of ascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'region beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-region-atpt (&optional beg end) 
"Counts matches of blank in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-region-until-point (&optional beg end) 
"Counts matches of blank in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-region-atpt (&optional beg end) 
"Relation of blank in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'region beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-region-atpt (&optional beg end) 
"Counts matches of cntrl in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-region-until-point (&optional beg end) 
"Counts matches of cntrl in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-region-atpt (&optional beg end) 
"Relation of cntrl in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'region beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-region-atpt (&optional beg end) 
"Counts matches of digit in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-region-until-point (&optional beg end) 
"Counts matches of digit in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-region-atpt (&optional beg end) 
"Relation of digit in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'region beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-region-atpt (&optional beg end) 
"Counts matches of graph in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-region-until-point (&optional beg end) 
"Counts matches of graph in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-region-atpt (&optional beg end) 
"Relation of graph in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'region beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-region-atpt (&optional beg end) 
"Counts matches of lower in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-region-until-point (&optional beg end) 
"Counts matches of lower in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-region-atpt (&optional beg end) 
"Relation of lower in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'region beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-region-atpt (&optional beg end) 
"Counts matches of nonascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-region-until-point (&optional beg end) 
"Counts matches of nonascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-region-atpt (&optional beg end) 
"Relation of nonascii in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'region beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-region-atpt (&optional beg end) 
"Counts matches of print in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-region-until-point (&optional beg end) 
"Counts matches of print in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-region-atpt (&optional beg end) 
"Relation of print in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'region beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-region-atpt (&optional beg end) 
"Counts matches of punct in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-region-until-point (&optional beg end) 
"Counts matches of punct in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-region-atpt (&optional beg end) 
"Relation of punct in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'region beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-region-atpt (&optional beg end) 
"Counts matches of space in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-region-until-point (&optional beg end) 
"Counts matches of space in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-region-atpt (&optional beg end) 
"Relation of space in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'region beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-region-atpt (&optional beg end) 
"Counts matches of upper in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-region-until-point (&optional beg end) 
"Counts matches of upper in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-region-atpt (&optional beg end) 
"Relation of upper in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'region beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-sentence-atpt (&optional beg end) 
"Counts matches of alnum in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-sentence-until-point (&optional beg end) 
"Counts matches of alnum in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-sentence-atpt (&optional beg end) 
"Relation of alnum in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-sentence-atpt (&optional beg end) 
"Counts matches of alpha in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-sentence-until-point (&optional beg end) 
"Counts matches of alpha in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-sentence-atpt (&optional beg end) 
"Relation of alpha in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-sentence-atpt (&optional beg end) 
"Counts matches of ascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-sentence-until-point (&optional beg end) 
"Counts matches of ascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-sentence-atpt (&optional beg end) 
"Relation of ascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-sentence-atpt (&optional beg end) 
"Counts matches of blank in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-sentence-until-point (&optional beg end) 
"Counts matches of blank in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-sentence-atpt (&optional beg end) 
"Relation of blank in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-sentence-atpt (&optional beg end) 
"Counts matches of cntrl in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-sentence-until-point (&optional beg end) 
"Counts matches of cntrl in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-sentence-atpt (&optional beg end) 
"Relation of cntrl in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-sentence-atpt (&optional beg end) 
"Counts matches of digit in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-sentence-until-point (&optional beg end) 
"Counts matches of digit in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-sentence-atpt (&optional beg end) 
"Relation of digit in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-sentence-atpt (&optional beg end) 
"Counts matches of graph in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-sentence-until-point (&optional beg end) 
"Counts matches of graph in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-sentence-atpt (&optional beg end) 
"Relation of graph in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-sentence-atpt (&optional beg end) 
"Counts matches of lower in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-sentence-until-point (&optional beg end) 
"Counts matches of lower in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-sentence-atpt (&optional beg end) 
"Relation of lower in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-sentence-atpt (&optional beg end) 
"Counts matches of nonascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-sentence-until-point (&optional beg end) 
"Counts matches of nonascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-sentence-atpt (&optional beg end) 
"Relation of nonascii in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-sentence-atpt (&optional beg end) 
"Counts matches of print in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-sentence-until-point (&optional beg end) 
"Counts matches of print in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-sentence-atpt (&optional beg end) 
"Relation of print in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-sentence-atpt (&optional beg end) 
"Counts matches of punct in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-sentence-until-point (&optional beg end) 
"Counts matches of punct in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-sentence-atpt (&optional beg end) 
"Relation of punct in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-sentence-atpt (&optional beg end) 
"Counts matches of space in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-sentence-until-point (&optional beg end) 
"Counts matches of space in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-sentence-atpt (&optional beg end) 
"Relation of space in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-sentence-atpt (&optional beg end) 
"Counts matches of upper in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-sentence-until-point (&optional beg end) 
"Counts matches of upper in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-sentence-atpt (&optional beg end) 
"Relation of upper in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-sexp-atpt (&optional beg end) 
"Counts matches of alnum in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-sexp-until-point (&optional beg end) 
"Counts matches of alnum in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-sexp-atpt (&optional beg end) 
"Relation of alnum in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-sexp-atpt (&optional beg end) 
"Counts matches of alpha in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-sexp-until-point (&optional beg end) 
"Counts matches of alpha in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-sexp-atpt (&optional beg end) 
"Relation of alpha in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-sexp-atpt (&optional beg end) 
"Counts matches of ascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-sexp-until-point (&optional beg end) 
"Counts matches of ascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-sexp-atpt (&optional beg end) 
"Relation of ascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-sexp-atpt (&optional beg end) 
"Counts matches of blank in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-sexp-until-point (&optional beg end) 
"Counts matches of blank in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-sexp-atpt (&optional beg end) 
"Relation of blank in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-sexp-atpt (&optional beg end) 
"Counts matches of cntrl in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-sexp-until-point (&optional beg end) 
"Counts matches of cntrl in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-sexp-atpt (&optional beg end) 
"Relation of cntrl in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-sexp-atpt (&optional beg end) 
"Counts matches of digit in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-sexp-until-point (&optional beg end) 
"Counts matches of digit in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-sexp-atpt (&optional beg end) 
"Relation of digit in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-sexp-atpt (&optional beg end) 
"Counts matches of graph in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-sexp-until-point (&optional beg end) 
"Counts matches of graph in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-sexp-atpt (&optional beg end) 
"Relation of graph in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-sexp-atpt (&optional beg end) 
"Counts matches of lower in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-sexp-until-point (&optional beg end) 
"Counts matches of lower in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-sexp-atpt (&optional beg end) 
"Relation of lower in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-sexp-atpt (&optional beg end) 
"Counts matches of nonascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-sexp-until-point (&optional beg end) 
"Counts matches of nonascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-sexp-atpt (&optional beg end) 
"Relation of nonascii in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-sexp-atpt (&optional beg end) 
"Counts matches of print in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-sexp-until-point (&optional beg end) 
"Counts matches of print in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-sexp-atpt (&optional beg end) 
"Relation of print in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-sexp-atpt (&optional beg end) 
"Counts matches of punct in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-sexp-until-point (&optional beg end) 
"Counts matches of punct in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-sexp-atpt (&optional beg end) 
"Relation of punct in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-sexp-atpt (&optional beg end) 
"Counts matches of space in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-sexp-until-point (&optional beg end) 
"Counts matches of space in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-sexp-atpt (&optional beg end) 
"Relation of space in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-sexp-atpt (&optional beg end) 
"Counts matches of upper in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sexp beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-sexp-until-point (&optional beg end) 
"Counts matches of upper in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sexp beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-sexp-atpt (&optional beg end) 
"Relation of upper in sexp if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'sexp beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-shstruct-atpt (&optional beg end) 
"Counts matches of alnum in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-shstruct-until-point (&optional beg end) 
"Counts matches of alnum in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-shstruct-atpt (&optional beg end) 
"Relation of alnum in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-shstruct-atpt (&optional beg end) 
"Counts matches of alpha in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-shstruct-until-point (&optional beg end) 
"Counts matches of alpha in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-shstruct-atpt (&optional beg end) 
"Relation of alpha in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-shstruct-atpt (&optional beg end) 
"Counts matches of ascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-shstruct-until-point (&optional beg end) 
"Counts matches of ascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-shstruct-atpt (&optional beg end) 
"Relation of ascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-shstruct-atpt (&optional beg end) 
"Counts matches of blank in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-shstruct-until-point (&optional beg end) 
"Counts matches of blank in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-shstruct-atpt (&optional beg end) 
"Relation of blank in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-shstruct-atpt (&optional beg end) 
"Counts matches of cntrl in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-shstruct-until-point (&optional beg end) 
"Counts matches of cntrl in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-shstruct-atpt (&optional beg end) 
"Relation of cntrl in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-shstruct-atpt (&optional beg end) 
"Counts matches of digit in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-shstruct-until-point (&optional beg end) 
"Counts matches of digit in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-shstruct-atpt (&optional beg end) 
"Relation of digit in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-shstruct-atpt (&optional beg end) 
"Counts matches of graph in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-shstruct-until-point (&optional beg end) 
"Counts matches of graph in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-shstruct-atpt (&optional beg end) 
"Relation of graph in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-shstruct-atpt (&optional beg end) 
"Counts matches of lower in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-shstruct-until-point (&optional beg end) 
"Counts matches of lower in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-shstruct-atpt (&optional beg end) 
"Relation of lower in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-shstruct-atpt (&optional beg end) 
"Counts matches of nonascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-shstruct-until-point (&optional beg end) 
"Counts matches of nonascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-shstruct-atpt (&optional beg end) 
"Relation of nonascii in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-shstruct-atpt (&optional beg end) 
"Counts matches of print in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-shstruct-until-point (&optional beg end) 
"Counts matches of print in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-shstruct-atpt (&optional beg end) 
"Relation of print in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-shstruct-atpt (&optional beg end) 
"Counts matches of punct in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-shstruct-until-point (&optional beg end) 
"Counts matches of punct in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-shstruct-atpt (&optional beg end) 
"Relation of punct in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-shstruct-atpt (&optional beg end) 
"Counts matches of space in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-shstruct-until-point (&optional beg end) 
"Counts matches of space in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-shstruct-atpt (&optional beg end) 
"Relation of space in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-shstruct-atpt (&optional beg end) 
"Counts matches of upper in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'shstruct beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-shstruct-until-point (&optional beg end) 
"Counts matches of upper in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'shstruct beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-shstruct-atpt (&optional beg end) 
"Relation of upper in shstruct if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'shstruct beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-symbol-atpt (&optional beg end) 
"Counts matches of alnum in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-symbol-until-point (&optional beg end) 
"Counts matches of alnum in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-symbol-atpt (&optional beg end) 
"Relation of alnum in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-symbol-atpt (&optional beg end) 
"Counts matches of alpha in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-symbol-until-point (&optional beg end) 
"Counts matches of alpha in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-symbol-atpt (&optional beg end) 
"Relation of alpha in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-symbol-atpt (&optional beg end) 
"Counts matches of ascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-symbol-until-point (&optional beg end) 
"Counts matches of ascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-symbol-atpt (&optional beg end) 
"Relation of ascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-symbol-atpt (&optional beg end) 
"Counts matches of blank in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-symbol-until-point (&optional beg end) 
"Counts matches of blank in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-symbol-atpt (&optional beg end) 
"Relation of blank in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-symbol-atpt (&optional beg end) 
"Counts matches of cntrl in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-symbol-until-point (&optional beg end) 
"Counts matches of cntrl in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-symbol-atpt (&optional beg end) 
"Relation of cntrl in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-symbol-atpt (&optional beg end) 
"Counts matches of digit in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-symbol-until-point (&optional beg end) 
"Counts matches of digit in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-symbol-atpt (&optional beg end) 
"Relation of digit in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-symbol-atpt (&optional beg end) 
"Counts matches of graph in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-symbol-until-point (&optional beg end) 
"Counts matches of graph in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-symbol-atpt (&optional beg end) 
"Relation of graph in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-symbol-atpt (&optional beg end) 
"Counts matches of lower in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-symbol-until-point (&optional beg end) 
"Counts matches of lower in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-symbol-atpt (&optional beg end) 
"Relation of lower in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-symbol-atpt (&optional beg end) 
"Counts matches of nonascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-symbol-until-point (&optional beg end) 
"Counts matches of nonascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-symbol-atpt (&optional beg end) 
"Relation of nonascii in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-symbol-atpt (&optional beg end) 
"Counts matches of print in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-symbol-until-point (&optional beg end) 
"Counts matches of print in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-symbol-atpt (&optional beg end) 
"Relation of print in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-symbol-atpt (&optional beg end) 
"Counts matches of punct in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-symbol-until-point (&optional beg end) 
"Counts matches of punct in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-symbol-atpt (&optional beg end) 
"Relation of punct in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-symbol-atpt (&optional beg end) 
"Counts matches of space in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-symbol-until-point (&optional beg end) 
"Counts matches of space in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-symbol-atpt (&optional beg end) 
"Relation of space in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-symbol-atpt (&optional beg end) 
"Counts matches of upper in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'symbol beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-symbol-until-point (&optional beg end) 
"Counts matches of upper in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'symbol beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-symbol-atpt (&optional beg end) 
"Relation of upper in symbol if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'symbol beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-url-atpt (&optional beg end) 
"Counts matches of alnum in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-url-until-point (&optional beg end) 
"Counts matches of alnum in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-url-atpt (&optional beg end) 
"Relation of alnum in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'url beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-url-atpt (&optional beg end) 
"Counts matches of alpha in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-url-until-point (&optional beg end) 
"Counts matches of alpha in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-url-atpt (&optional beg end) 
"Relation of alpha in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'url beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-url-atpt (&optional beg end) 
"Counts matches of ascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-url-until-point (&optional beg end) 
"Counts matches of ascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-url-atpt (&optional beg end) 
"Relation of ascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'url beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-url-atpt (&optional beg end) 
"Counts matches of blank in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-url-until-point (&optional beg end) 
"Counts matches of blank in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-url-atpt (&optional beg end) 
"Relation of blank in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'url beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-url-atpt (&optional beg end) 
"Counts matches of cntrl in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-url-until-point (&optional beg end) 
"Counts matches of cntrl in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-url-atpt (&optional beg end) 
"Relation of cntrl in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'url beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-url-atpt (&optional beg end) 
"Counts matches of digit in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-url-until-point (&optional beg end) 
"Counts matches of digit in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-url-atpt (&optional beg end) 
"Relation of digit in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'url beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-url-atpt (&optional beg end) 
"Counts matches of graph in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-url-until-point (&optional beg end) 
"Counts matches of graph in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-url-atpt (&optional beg end) 
"Relation of graph in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'url beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-url-atpt (&optional beg end) 
"Counts matches of lower in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-url-until-point (&optional beg end) 
"Counts matches of lower in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-url-atpt (&optional beg end) 
"Relation of lower in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'url beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-url-atpt (&optional beg end) 
"Counts matches of nonascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-url-until-point (&optional beg end) 
"Counts matches of nonascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-url-atpt (&optional beg end) 
"Relation of nonascii in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'url beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-url-atpt (&optional beg end) 
"Counts matches of print in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-url-until-point (&optional beg end) 
"Counts matches of print in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-url-atpt (&optional beg end) 
"Relation of print in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'url beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-url-atpt (&optional beg end) 
"Counts matches of punct in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-url-until-point (&optional beg end) 
"Counts matches of punct in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-url-atpt (&optional beg end) 
"Relation of punct in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'url beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-url-atpt (&optional beg end) 
"Counts matches of space in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-url-until-point (&optional beg end) 
"Counts matches of space in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-url-atpt (&optional beg end) 
"Relation of space in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'url beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-url-atpt (&optional beg end) 
"Counts matches of upper in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'url beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-url-until-point (&optional beg end) 
"Counts matches of upper in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'url beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-url-atpt (&optional beg end) 
"Relation of upper in url if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'url beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-word-atpt (&optional beg end) 
"Counts matches of alnum in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-word-until-point (&optional beg end) 
"Counts matches of alnum in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-word-atpt (&optional beg end) 
"Relation of alnum in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'word beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-word-atpt (&optional beg end) 
"Counts matches of alpha in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-word-until-point (&optional beg end) 
"Counts matches of alpha in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-word-atpt (&optional beg end) 
"Relation of alpha in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'word beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-word-atpt (&optional beg end) 
"Counts matches of ascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-word-until-point (&optional beg end) 
"Counts matches of ascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-word-atpt (&optional beg end) 
"Relation of ascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'word beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-word-atpt (&optional beg end) 
"Counts matches of blank in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-word-until-point (&optional beg end) 
"Counts matches of blank in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-word-atpt (&optional beg end) 
"Relation of blank in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'word beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-word-atpt (&optional beg end) 
"Counts matches of cntrl in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-word-until-point (&optional beg end) 
"Counts matches of cntrl in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-word-atpt (&optional beg end) 
"Relation of cntrl in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'word beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-word-atpt (&optional beg end) 
"Counts matches of digit in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-word-until-point (&optional beg end) 
"Counts matches of digit in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-word-atpt (&optional beg end) 
"Relation of digit in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'word beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-word-atpt (&optional beg end) 
"Counts matches of graph in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-word-until-point (&optional beg end) 
"Counts matches of graph in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-word-atpt (&optional beg end) 
"Relation of graph in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'word beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-word-atpt (&optional beg end) 
"Counts matches of lower in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-word-until-point (&optional beg end) 
"Counts matches of lower in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-word-atpt (&optional beg end) 
"Relation of lower in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'word beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-word-atpt (&optional beg end) 
"Counts matches of nonascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-word-until-point (&optional beg end) 
"Counts matches of nonascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-word-atpt (&optional beg end) 
"Relation of nonascii in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'word beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-word-atpt (&optional beg end) 
"Counts matches of print in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-word-until-point (&optional beg end) 
"Counts matches of print in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-word-atpt (&optional beg end) 
"Relation of print in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'word beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-word-atpt (&optional beg end) 
"Counts matches of punct in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-word-until-point (&optional beg end) 
"Counts matches of punct in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-word-atpt (&optional beg end) 
"Relation of punct in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'word beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-word-atpt (&optional beg end) 
"Counts matches of space in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-word-until-point (&optional beg end) 
"Counts matches of space in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-word-atpt (&optional beg end) 
"Relation of space in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'word beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-word-atpt (&optional beg end) 
"Counts matches of upper in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'word beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-word-until-point (&optional beg end) 
"Counts matches of upper in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'word beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-word-atpt (&optional beg end) 
"Relation of upper in word if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'word beg end t (called-interactively-p 'any)))


;;; count/ratio ar-atpt-rest-list ar-atpt-classes end


;;; count/ratio ar-paired-delimited-passiv ar-atpt-classes start

(defun ar-count-alnum-in-braced-atpt (&optional beg end) 
"Counts matches of alnum in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-braced-until-point (&optional beg end) 
"Counts matches of alnum in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-braced-atpt (&optional beg end) 
"Relation of alnum in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-braced-atpt (&optional beg end) 
"Counts matches of alpha in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-braced-until-point (&optional beg end) 
"Counts matches of alpha in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-braced-atpt (&optional beg end) 
"Relation of alpha in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-braced-atpt (&optional beg end) 
"Counts matches of ascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-braced-until-point (&optional beg end) 
"Counts matches of ascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-braced-atpt (&optional beg end) 
"Relation of ascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-braced-atpt (&optional beg end) 
"Counts matches of blank in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-braced-until-point (&optional beg end) 
"Counts matches of blank in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-braced-atpt (&optional beg end) 
"Relation of blank in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-braced-atpt (&optional beg end) 
"Counts matches of cntrl in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-braced-until-point (&optional beg end) 
"Counts matches of cntrl in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-braced-atpt (&optional beg end) 
"Relation of cntrl in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-braced-atpt (&optional beg end) 
"Counts matches of digit in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-braced-until-point (&optional beg end) 
"Counts matches of digit in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-braced-atpt (&optional beg end) 
"Relation of digit in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-braced-atpt (&optional beg end) 
"Counts matches of graph in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-braced-until-point (&optional beg end) 
"Counts matches of graph in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-braced-atpt (&optional beg end) 
"Relation of graph in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-braced-atpt (&optional beg end) 
"Counts matches of lower in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-braced-until-point (&optional beg end) 
"Counts matches of lower in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-braced-atpt (&optional beg end) 
"Relation of lower in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-braced-atpt (&optional beg end) 
"Counts matches of nonascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-braced-until-point (&optional beg end) 
"Counts matches of nonascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-braced-atpt (&optional beg end) 
"Relation of nonascii in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-braced-atpt (&optional beg end) 
"Counts matches of print in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-braced-until-point (&optional beg end) 
"Counts matches of print in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-braced-atpt (&optional beg end) 
"Relation of print in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-braced-atpt (&optional beg end) 
"Counts matches of punct in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-braced-until-point (&optional beg end) 
"Counts matches of punct in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-braced-atpt (&optional beg end) 
"Relation of punct in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-braced-atpt (&optional beg end) 
"Counts matches of space in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-braced-until-point (&optional beg end) 
"Counts matches of space in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-braced-atpt (&optional beg end) 
"Relation of space in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-braced-atpt (&optional beg end) 
"Counts matches of upper in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-braced-until-point (&optional beg end) 
"Counts matches of upper in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-braced-atpt (&optional beg end) 
"Relation of upper in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-bracketed-atpt (&optional beg end) 
"Counts matches of alnum in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-bracketed-until-point (&optional beg end) 
"Counts matches of alnum in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-bracketed-atpt (&optional beg end) 
"Relation of alnum in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-bracketed-atpt (&optional beg end) 
"Counts matches of alpha in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-bracketed-until-point (&optional beg end) 
"Counts matches of alpha in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-bracketed-atpt (&optional beg end) 
"Relation of alpha in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-bracketed-atpt (&optional beg end) 
"Counts matches of ascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-bracketed-until-point (&optional beg end) 
"Counts matches of ascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-bracketed-atpt (&optional beg end) 
"Relation of ascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-bracketed-atpt (&optional beg end) 
"Counts matches of blank in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-bracketed-until-point (&optional beg end) 
"Counts matches of blank in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-bracketed-atpt (&optional beg end) 
"Relation of blank in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-bracketed-atpt (&optional beg end) 
"Counts matches of cntrl in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-bracketed-until-point (&optional beg end) 
"Counts matches of cntrl in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-bracketed-atpt (&optional beg end) 
"Relation of cntrl in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-bracketed-atpt (&optional beg end) 
"Counts matches of digit in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-bracketed-until-point (&optional beg end) 
"Counts matches of digit in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-bracketed-atpt (&optional beg end) 
"Relation of digit in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-bracketed-atpt (&optional beg end) 
"Counts matches of graph in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-bracketed-until-point (&optional beg end) 
"Counts matches of graph in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-bracketed-atpt (&optional beg end) 
"Relation of graph in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-bracketed-atpt (&optional beg end) 
"Counts matches of lower in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-bracketed-until-point (&optional beg end) 
"Counts matches of lower in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-bracketed-atpt (&optional beg end) 
"Relation of lower in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-bracketed-atpt (&optional beg end) 
"Counts matches of nonascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-bracketed-until-point (&optional beg end) 
"Counts matches of nonascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-bracketed-atpt (&optional beg end) 
"Relation of nonascii in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-bracketed-atpt (&optional beg end) 
"Counts matches of print in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-bracketed-until-point (&optional beg end) 
"Counts matches of print in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-bracketed-atpt (&optional beg end) 
"Relation of print in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-bracketed-atpt (&optional beg end) 
"Counts matches of punct in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-bracketed-until-point (&optional beg end) 
"Counts matches of punct in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-bracketed-atpt (&optional beg end) 
"Relation of punct in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-bracketed-atpt (&optional beg end) 
"Counts matches of space in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-bracketed-until-point (&optional beg end) 
"Counts matches of space in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-bracketed-atpt (&optional beg end) 
"Relation of space in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-bracketed-atpt (&optional beg end) 
"Counts matches of upper in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-bracketed-until-point (&optional beg end) 
"Counts matches of upper in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-bracketed-atpt (&optional beg end) 
"Relation of upper in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-lesserangled-atpt (&optional beg end) 
"Counts matches of alnum in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-lesserangled-until-point (&optional beg end) 
"Counts matches of alnum in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-lesserangled-atpt (&optional beg end) 
"Relation of alnum in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-lesserangled-atpt (&optional beg end) 
"Counts matches of alpha in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-lesserangled-until-point (&optional beg end) 
"Counts matches of alpha in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-lesserangled-atpt (&optional beg end) 
"Relation of alpha in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-lesserangled-atpt (&optional beg end) 
"Counts matches of ascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-lesserangled-until-point (&optional beg end) 
"Counts matches of ascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-lesserangled-atpt (&optional beg end) 
"Relation of ascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-lesserangled-atpt (&optional beg end) 
"Counts matches of blank in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-lesserangled-until-point (&optional beg end) 
"Counts matches of blank in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-lesserangled-atpt (&optional beg end) 
"Relation of blank in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-lesserangled-atpt (&optional beg end) 
"Counts matches of cntrl in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-lesserangled-until-point (&optional beg end) 
"Counts matches of cntrl in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-lesserangled-atpt (&optional beg end) 
"Relation of cntrl in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-lesserangled-atpt (&optional beg end) 
"Counts matches of digit in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-lesserangled-until-point (&optional beg end) 
"Counts matches of digit in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-lesserangled-atpt (&optional beg end) 
"Relation of digit in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-lesserangled-atpt (&optional beg end) 
"Counts matches of graph in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-lesserangled-until-point (&optional beg end) 
"Counts matches of graph in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-lesserangled-atpt (&optional beg end) 
"Relation of graph in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-lesserangled-atpt (&optional beg end) 
"Counts matches of lower in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-lesserangled-until-point (&optional beg end) 
"Counts matches of lower in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-lesserangled-atpt (&optional beg end) 
"Relation of lower in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-lesserangled-atpt (&optional beg end) 
"Counts matches of nonascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-lesserangled-until-point (&optional beg end) 
"Counts matches of nonascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-lesserangled-atpt (&optional beg end) 
"Relation of nonascii in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-lesserangled-atpt (&optional beg end) 
"Counts matches of print in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-lesserangled-until-point (&optional beg end) 
"Counts matches of print in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-lesserangled-atpt (&optional beg end) 
"Relation of print in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-lesserangled-atpt (&optional beg end) 
"Counts matches of punct in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-lesserangled-until-point (&optional beg end) 
"Counts matches of punct in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-lesserangled-atpt (&optional beg end) 
"Relation of punct in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-lesserangled-atpt (&optional beg end) 
"Counts matches of space in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-lesserangled-until-point (&optional beg end) 
"Counts matches of space in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-lesserangled-atpt (&optional beg end) 
"Relation of space in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-lesserangled-atpt (&optional beg end) 
"Counts matches of upper in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-lesserangled-until-point (&optional beg end) 
"Counts matches of upper in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-lesserangled-atpt (&optional beg end) 
"Relation of upper in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-greaterangled-atpt (&optional beg end) 
"Counts matches of alnum in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-greaterangled-until-point (&optional beg end) 
"Counts matches of alnum in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-greaterangled-atpt (&optional beg end) 
"Relation of alnum in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-greaterangled-atpt (&optional beg end) 
"Counts matches of alpha in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-greaterangled-until-point (&optional beg end) 
"Counts matches of alpha in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-greaterangled-atpt (&optional beg end) 
"Relation of alpha in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-greaterangled-atpt (&optional beg end) 
"Counts matches of ascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-greaterangled-until-point (&optional beg end) 
"Counts matches of ascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-greaterangled-atpt (&optional beg end) 
"Relation of ascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-greaterangled-atpt (&optional beg end) 
"Counts matches of blank in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-greaterangled-until-point (&optional beg end) 
"Counts matches of blank in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-greaterangled-atpt (&optional beg end) 
"Relation of blank in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-greaterangled-atpt (&optional beg end) 
"Counts matches of cntrl in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-greaterangled-until-point (&optional beg end) 
"Counts matches of cntrl in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-greaterangled-atpt (&optional beg end) 
"Relation of cntrl in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-greaterangled-atpt (&optional beg end) 
"Counts matches of digit in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-greaterangled-until-point (&optional beg end) 
"Counts matches of digit in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-greaterangled-atpt (&optional beg end) 
"Relation of digit in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-greaterangled-atpt (&optional beg end) 
"Counts matches of graph in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-greaterangled-until-point (&optional beg end) 
"Counts matches of graph in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-greaterangled-atpt (&optional beg end) 
"Relation of graph in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-greaterangled-atpt (&optional beg end) 
"Counts matches of lower in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-greaterangled-until-point (&optional beg end) 
"Counts matches of lower in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-greaterangled-atpt (&optional beg end) 
"Relation of lower in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-greaterangled-atpt (&optional beg end) 
"Counts matches of nonascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-greaterangled-until-point (&optional beg end) 
"Counts matches of nonascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-greaterangled-atpt (&optional beg end) 
"Relation of nonascii in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-greaterangled-atpt (&optional beg end) 
"Counts matches of print in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-greaterangled-until-point (&optional beg end) 
"Counts matches of print in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-greaterangled-atpt (&optional beg end) 
"Relation of print in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-greaterangled-atpt (&optional beg end) 
"Counts matches of punct in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-greaterangled-until-point (&optional beg end) 
"Counts matches of punct in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-greaterangled-atpt (&optional beg end) 
"Relation of punct in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-greaterangled-atpt (&optional beg end) 
"Counts matches of space in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-greaterangled-until-point (&optional beg end) 
"Counts matches of space in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-greaterangled-atpt (&optional beg end) 
"Relation of space in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-greaterangled-atpt (&optional beg end) 
"Counts matches of upper in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-greaterangled-until-point (&optional beg end) 
"Counts matches of upper in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-greaterangled-atpt (&optional beg end) 
"Relation of upper in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of alnum in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of alnum in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of alnum in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of alpha in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of alpha in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of alpha in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of ascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of ascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of ascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of blank in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of blank in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of blank in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of cntrl in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of cntrl in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of cntrl in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of digit in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of digit in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of digit in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of graph in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of graph in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of graph in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of lower in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of lower in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of lower in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of nonascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of nonascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of nonascii in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of print in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of print in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of print in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of punct in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of punct in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of punct in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of space in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of space in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of space in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of upper in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of upper in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of upper in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of alnum in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of alnum in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of alnum in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of alpha in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of alpha in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of alpha in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of ascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of ascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of ascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of blank in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of blank in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of blank in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of cntrl in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of cntrl in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of cntrl in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of digit in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of digit in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of digit in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of graph in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of graph in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of graph in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of lower in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of lower in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of lower in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of nonascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of nonascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of nonascii in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of print in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of print in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of print in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of punct in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of punct in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of punct in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of space in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of space in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of space in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of upper in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of upper in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of upper in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-alnum-in-parentized-atpt (&optional beg end) 
"Counts matches of alnum in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-alnum-in-parentized-until-point (&optional beg end) 
"Counts matches of alnum in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alnum-in-parentized-atpt (&optional beg end) 
"Relation of alnum in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alnum 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-alpha-in-parentized-atpt (&optional beg end) 
"Counts matches of alpha in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-alpha-in-parentized-until-point (&optional beg end) 
"Counts matches of alpha in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-alpha-in-parentized-atpt (&optional beg end) 
"Relation of alpha in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'alpha 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-ascii-in-parentized-atpt (&optional beg end) 
"Counts matches of ascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-ascii-in-parentized-until-point (&optional beg end) 
"Counts matches of ascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-ascii-in-parentized-atpt (&optional beg end) 
"Relation of ascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'ascii 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-blank-in-parentized-atpt (&optional beg end) 
"Counts matches of blank in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-blank-in-parentized-until-point (&optional beg end) 
"Counts matches of blank in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-blank-in-parentized-atpt (&optional beg end) 
"Relation of blank in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'blank 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-cntrl-in-parentized-atpt (&optional beg end) 
"Counts matches of cntrl in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-cntrl-in-parentized-until-point (&optional beg end) 
"Counts matches of cntrl in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-cntrl-in-parentized-atpt (&optional beg end) 
"Relation of cntrl in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'cntrl 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-digit-in-parentized-atpt (&optional beg end) 
"Counts matches of digit in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-digit-in-parentized-until-point (&optional beg end) 
"Counts matches of digit in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-digit-in-parentized-atpt (&optional beg end) 
"Relation of digit in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'digit 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-graph-in-parentized-atpt (&optional beg end) 
"Counts matches of graph in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-graph-in-parentized-until-point (&optional beg end) 
"Counts matches of graph in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-graph-in-parentized-atpt (&optional beg end) 
"Relation of graph in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'graph 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-lower-in-parentized-atpt (&optional beg end) 
"Counts matches of lower in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-lower-in-parentized-until-point (&optional beg end) 
"Counts matches of lower in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lower-in-parentized-atpt (&optional beg end) 
"Relation of lower in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lower 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-nonascii-in-parentized-atpt (&optional beg end) 
"Counts matches of nonascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-nonascii-in-parentized-until-point (&optional beg end) 
"Counts matches of nonascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-nonascii-in-parentized-atpt (&optional beg end) 
"Relation of nonascii in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'nonascii 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-print-in-parentized-atpt (&optional beg end) 
"Counts matches of print in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-print-in-parentized-until-point (&optional beg end) 
"Counts matches of print in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-print-in-parentized-atpt (&optional beg end) 
"Relation of print in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'print 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-punct-in-parentized-atpt (&optional beg end) 
"Counts matches of punct in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-punct-in-parentized-until-point (&optional beg end) 
"Counts matches of punct in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-punct-in-parentized-atpt (&optional beg end) 
"Relation of punct in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'punct 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-space-in-parentized-atpt (&optional beg end) 
"Counts matches of space in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-space-in-parentized-until-point (&optional beg end) 
"Counts matches of space in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-space-in-parentized-atpt (&optional beg end) 
"Relation of space in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'space 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-upper-in-parentized-atpt (&optional beg end) 
"Counts matches of upper in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-upper-in-parentized-until-point (&optional beg end) 
"Counts matches of upper in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-upper-in-parentized-atpt (&optional beg end) 
"Relation of upper in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'upper 'parentized beg end t (called-interactively-p 'any)))


;;; count/ratio ar-paired-delimited-passiv ar-atpt-classes end


;;; count/ratio ar-paired-delimited-passiv ar-paired-delimited-passiv start

(defun ar-count-braced-in-braced-atpt (&optional beg end) 
"Counts matches of braced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-braced-until-point (&optional beg end) 
"Counts matches of braced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-braced-atpt (&optional beg end) 
"Relation of braced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-braced-atpt (&optional beg end) 
"Counts matches of bracketed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-braced-until-point (&optional beg end) 
"Counts matches of bracketed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-braced-atpt (&optional beg end) 
"Relation of bracketed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-braced-atpt (&optional beg end) 
"Counts matches of lesserangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-braced-until-point (&optional beg end) 
"Counts matches of lesserangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-braced-atpt (&optional beg end) 
"Relation of lesserangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-braced-atpt (&optional beg end) 
"Counts matches of greaterangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-braced-until-point (&optional beg end) 
"Counts matches of greaterangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-braced-atpt (&optional beg end) 
"Relation of greaterangled in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-braced-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-braced-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-braced-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-braced-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-braced-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-braced-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-braced-atpt (&optional beg end) 
"Counts matches of parentized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-braced-until-point (&optional beg end) 
"Counts matches of parentized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-braced-atpt (&optional beg end) 
"Relation of parentized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-bracketed-atpt (&optional beg end) 
"Counts matches of braced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-bracketed-until-point (&optional beg end) 
"Counts matches of braced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-bracketed-atpt (&optional beg end) 
"Relation of braced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-bracketed-atpt (&optional beg end) 
"Counts matches of bracketed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-bracketed-until-point (&optional beg end) 
"Counts matches of bracketed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-bracketed-atpt (&optional beg end) 
"Relation of bracketed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-bracketed-atpt (&optional beg end) 
"Counts matches of lesserangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-bracketed-until-point (&optional beg end) 
"Counts matches of lesserangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-bracketed-atpt (&optional beg end) 
"Relation of lesserangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-bracketed-atpt (&optional beg end) 
"Counts matches of greaterangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-bracketed-until-point (&optional beg end) 
"Counts matches of greaterangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-bracketed-atpt (&optional beg end) 
"Relation of greaterangled in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-bracketed-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-bracketed-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-bracketed-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-bracketed-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-bracketed-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-bracketed-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-bracketed-atpt (&optional beg end) 
"Counts matches of parentized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-bracketed-until-point (&optional beg end) 
"Counts matches of parentized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-bracketed-atpt (&optional beg end) 
"Relation of parentized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-lesserangled-atpt (&optional beg end) 
"Counts matches of braced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-lesserangled-until-point (&optional beg end) 
"Counts matches of braced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-lesserangled-atpt (&optional beg end) 
"Relation of braced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-lesserangled-atpt (&optional beg end) 
"Counts matches of bracketed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-lesserangled-until-point (&optional beg end) 
"Counts matches of bracketed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-lesserangled-atpt (&optional beg end) 
"Relation of bracketed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-lesserangled-atpt (&optional beg end) 
"Counts matches of lesserangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-lesserangled-until-point (&optional beg end) 
"Counts matches of lesserangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-lesserangled-atpt (&optional beg end) 
"Relation of lesserangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-lesserangled-atpt (&optional beg end) 
"Counts matches of greaterangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-lesserangled-until-point (&optional beg end) 
"Counts matches of greaterangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-lesserangled-atpt (&optional beg end) 
"Relation of greaterangled in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-lesserangled-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-lesserangled-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-lesserangled-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-lesserangled-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-lesserangled-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-lesserangled-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-lesserangled-atpt (&optional beg end) 
"Counts matches of parentized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-lesserangled-until-point (&optional beg end) 
"Counts matches of parentized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-lesserangled-atpt (&optional beg end) 
"Relation of parentized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-greaterangled-atpt (&optional beg end) 
"Counts matches of braced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-greaterangled-until-point (&optional beg end) 
"Counts matches of braced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-greaterangled-atpt (&optional beg end) 
"Relation of braced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-greaterangled-atpt (&optional beg end) 
"Counts matches of bracketed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-greaterangled-until-point (&optional beg end) 
"Counts matches of bracketed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-greaterangled-atpt (&optional beg end) 
"Relation of bracketed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-greaterangled-atpt (&optional beg end) 
"Counts matches of lesserangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-greaterangled-until-point (&optional beg end) 
"Counts matches of lesserangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-greaterangled-atpt (&optional beg end) 
"Relation of lesserangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-greaterangled-atpt (&optional beg end) 
"Counts matches of greaterangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-greaterangled-until-point (&optional beg end) 
"Counts matches of greaterangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-greaterangled-atpt (&optional beg end) 
"Relation of greaterangled in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-greaterangled-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-greaterangled-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-greaterangled-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-greaterangled-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-greaterangled-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-greaterangled-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-greaterangled-atpt (&optional beg end) 
"Counts matches of parentized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-greaterangled-until-point (&optional beg end) 
"Counts matches of parentized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-greaterangled-atpt (&optional beg end) 
"Relation of parentized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of braced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of braced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of braced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of bracketed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of bracketed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of bracketed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of lesserangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of lesserangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of lesserangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of greaterangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of greaterangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of greaterangled in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of parentized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of parentized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of parentized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of braced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of braced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of braced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of bracketed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of bracketed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of bracketed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of lesserangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of lesserangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of lesserangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of greaterangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of greaterangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of greaterangled in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of parentized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of parentized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of parentized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-parentized-atpt (&optional beg end) 
"Counts matches of braced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-parentized-until-point (&optional beg end) 
"Counts matches of braced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-parentized-atpt (&optional beg end) 
"Relation of braced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-parentized-atpt (&optional beg end) 
"Counts matches of bracketed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-parentized-until-point (&optional beg end) 
"Counts matches of bracketed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-parentized-atpt (&optional beg end) 
"Relation of bracketed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-parentized-atpt (&optional beg end) 
"Counts matches of lesserangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-parentized-until-point (&optional beg end) 
"Counts matches of lesserangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-parentized-atpt (&optional beg end) 
"Relation of lesserangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-parentized-atpt (&optional beg end) 
"Counts matches of greaterangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-parentized-until-point (&optional beg end) 
"Counts matches of greaterangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-parentized-atpt (&optional beg end) 
"Relation of greaterangled in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-parentized-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-parentized-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-parentized-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-parentized-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-parentized-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-parentized-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-parentized-atpt (&optional beg end) 
"Counts matches of parentized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-parentized-until-point (&optional beg end) 
"Counts matches of parentized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-parentized-atpt (&optional beg end) 
"Relation of parentized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'parentized beg end t (called-interactively-p 'any)))


;;; count/ratio ar-paired-delimited-passiv ar-paired-delimited-passiv end


;;; count/ratio ar-unpaired-delimited-passiv ar-paired-delimited-passiv start

(defun ar-count-braced-in-backslashed-atpt (&optional beg end) 
"Counts matches of braced in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-backslashed-until-point (&optional beg end) 
"Counts matches of braced in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-backslashed-atpt (&optional beg end) 
"Relation of braced in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-backslashed-atpt (&optional beg end) 
"Counts matches of bracketed in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-backslashed-until-point (&optional beg end) 
"Counts matches of bracketed in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-backslashed-atpt (&optional beg end) 
"Relation of bracketed in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-backslashed-atpt (&optional beg end) 
"Counts matches of lesserangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-backslashed-until-point (&optional beg end) 
"Counts matches of lesserangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-backslashed-atpt (&optional beg end) 
"Relation of lesserangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-backslashed-atpt (&optional beg end) 
"Counts matches of greaterangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-backslashed-until-point (&optional beg end) 
"Counts matches of greaterangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-backslashed-atpt (&optional beg end) 
"Relation of greaterangled in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-backslashed-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-backslashed-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-backslashed-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-backslashed-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-backslashed-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-backslashed-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-backslashed-atpt (&optional beg end) 
"Counts matches of parentized in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backslashed beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-backslashed-until-point (&optional beg end) 
"Counts matches of parentized in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backslashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-backslashed-atpt (&optional beg end) 
"Relation of parentized in backslashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backslashed beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-backticked-atpt (&optional beg end) 
"Counts matches of braced in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-backticked-until-point (&optional beg end) 
"Counts matches of braced in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-backticked-atpt (&optional beg end) 
"Relation of braced in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-backticked-atpt (&optional beg end) 
"Counts matches of bracketed in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-backticked-until-point (&optional beg end) 
"Counts matches of bracketed in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-backticked-atpt (&optional beg end) 
"Relation of bracketed in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-backticked-atpt (&optional beg end) 
"Counts matches of lesserangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-backticked-until-point (&optional beg end) 
"Counts matches of lesserangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-backticked-atpt (&optional beg end) 
"Relation of lesserangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-backticked-atpt (&optional beg end) 
"Counts matches of greaterangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-backticked-until-point (&optional beg end) 
"Counts matches of greaterangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-backticked-atpt (&optional beg end) 
"Relation of greaterangled in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-backticked-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-backticked-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-backticked-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-backticked-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-backticked-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-backticked-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-backticked-atpt (&optional beg end) 
"Counts matches of parentized in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backticked beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-backticked-until-point (&optional beg end) 
"Counts matches of parentized in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backticked beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-backticked-atpt (&optional beg end) 
"Relation of parentized in backticked if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'backticked beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-coloned-atpt (&optional beg end) 
"Counts matches of braced in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-coloned-until-point (&optional beg end) 
"Counts matches of braced in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-coloned-atpt (&optional beg end) 
"Relation of braced in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-coloned-atpt (&optional beg end) 
"Counts matches of bracketed in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-coloned-until-point (&optional beg end) 
"Counts matches of bracketed in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-coloned-atpt (&optional beg end) 
"Relation of bracketed in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-coloned-atpt (&optional beg end) 
"Counts matches of lesserangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-coloned-until-point (&optional beg end) 
"Counts matches of lesserangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-coloned-atpt (&optional beg end) 
"Relation of lesserangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-coloned-atpt (&optional beg end) 
"Counts matches of greaterangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-coloned-until-point (&optional beg end) 
"Counts matches of greaterangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-coloned-atpt (&optional beg end) 
"Relation of greaterangled in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-coloned-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-coloned-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-coloned-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-coloned-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-coloned-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-coloned-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-coloned-atpt (&optional beg end) 
"Counts matches of parentized in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'coloned beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-coloned-until-point (&optional beg end) 
"Counts matches of parentized in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'coloned beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-coloned-atpt (&optional beg end) 
"Relation of parentized in coloned if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'coloned beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-crossed-atpt (&optional beg end) 
"Counts matches of braced in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-crossed-until-point (&optional beg end) 
"Counts matches of braced in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-crossed-atpt (&optional beg end) 
"Relation of braced in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-crossed-atpt (&optional beg end) 
"Counts matches of bracketed in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-crossed-until-point (&optional beg end) 
"Counts matches of bracketed in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-crossed-atpt (&optional beg end) 
"Relation of bracketed in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-crossed-atpt (&optional beg end) 
"Counts matches of lesserangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-crossed-until-point (&optional beg end) 
"Counts matches of lesserangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-crossed-atpt (&optional beg end) 
"Relation of lesserangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-crossed-atpt (&optional beg end) 
"Counts matches of greaterangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-crossed-until-point (&optional beg end) 
"Counts matches of greaterangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-crossed-atpt (&optional beg end) 
"Relation of greaterangled in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-crossed-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-crossed-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-crossed-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-crossed-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-crossed-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-crossed-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-crossed-atpt (&optional beg end) 
"Counts matches of parentized in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'crossed beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-crossed-until-point (&optional beg end) 
"Counts matches of parentized in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'crossed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-crossed-atpt (&optional beg end) 
"Relation of parentized in crossed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'crossed beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-dollared-atpt (&optional beg end) 
"Counts matches of braced in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-dollared-until-point (&optional beg end) 
"Counts matches of braced in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-dollared-atpt (&optional beg end) 
"Relation of braced in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-dollared-atpt (&optional beg end) 
"Counts matches of bracketed in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-dollared-until-point (&optional beg end) 
"Counts matches of bracketed in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-dollared-atpt (&optional beg end) 
"Relation of bracketed in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-dollared-atpt (&optional beg end) 
"Counts matches of lesserangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-dollared-until-point (&optional beg end) 
"Counts matches of lesserangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-dollared-atpt (&optional beg end) 
"Relation of lesserangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-dollared-atpt (&optional beg end) 
"Counts matches of greaterangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-dollared-until-point (&optional beg end) 
"Counts matches of greaterangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-dollared-atpt (&optional beg end) 
"Relation of greaterangled in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-dollared-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-dollared-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-dollared-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-dollared-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-dollared-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-dollared-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-dollared-atpt (&optional beg end) 
"Counts matches of parentized in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'dollared beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-dollared-until-point (&optional beg end) 
"Counts matches of parentized in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'dollared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-dollared-atpt (&optional beg end) 
"Relation of parentized in dollared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'dollared beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-doublequoted-atpt (&optional beg end) 
"Counts matches of braced in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-doublequoted-until-point (&optional beg end) 
"Counts matches of braced in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-doublequoted-atpt (&optional beg end) 
"Relation of braced in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-doublequoted-atpt (&optional beg end) 
"Counts matches of bracketed in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-doublequoted-until-point (&optional beg end) 
"Counts matches of bracketed in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-doublequoted-atpt (&optional beg end) 
"Relation of bracketed in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-doublequoted-atpt (&optional beg end) 
"Counts matches of lesserangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-doublequoted-until-point (&optional beg end) 
"Counts matches of lesserangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-doublequoted-atpt (&optional beg end) 
"Relation of lesserangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-doublequoted-atpt (&optional beg end) 
"Counts matches of greaterangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-doublequoted-until-point (&optional beg end) 
"Counts matches of greaterangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-doublequoted-atpt (&optional beg end) 
"Relation of greaterangled in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-doublequoted-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-doublequoted-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-doublequoted-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-doublequoted-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-doublequoted-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-doublequoted-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-doublequoted-atpt (&optional beg end) 
"Counts matches of parentized in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'doublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-doublequoted-until-point (&optional beg end) 
"Counts matches of parentized in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'doublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-doublequoted-atpt (&optional beg end) 
"Relation of parentized in doublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'doublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-equalized-atpt (&optional beg end) 
"Counts matches of braced in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-equalized-until-point (&optional beg end) 
"Counts matches of braced in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-equalized-atpt (&optional beg end) 
"Relation of braced in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-equalized-atpt (&optional beg end) 
"Counts matches of bracketed in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-equalized-until-point (&optional beg end) 
"Counts matches of bracketed in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-equalized-atpt (&optional beg end) 
"Relation of bracketed in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-equalized-atpt (&optional beg end) 
"Counts matches of lesserangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-equalized-until-point (&optional beg end) 
"Counts matches of lesserangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-equalized-atpt (&optional beg end) 
"Relation of lesserangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-equalized-atpt (&optional beg end) 
"Counts matches of greaterangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-equalized-until-point (&optional beg end) 
"Counts matches of greaterangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-equalized-atpt (&optional beg end) 
"Relation of greaterangled in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-equalized-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-equalized-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-equalized-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-equalized-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-equalized-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-equalized-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-equalized-atpt (&optional beg end) 
"Counts matches of parentized in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'equalized beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-equalized-until-point (&optional beg end) 
"Counts matches of parentized in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'equalized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-equalized-atpt (&optional beg end) 
"Relation of parentized in equalized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'equalized beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-hashed-atpt (&optional beg end) 
"Counts matches of braced in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-hashed-until-point (&optional beg end) 
"Counts matches of braced in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-hashed-atpt (&optional beg end) 
"Relation of braced in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-hashed-atpt (&optional beg end) 
"Counts matches of bracketed in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-hashed-until-point (&optional beg end) 
"Counts matches of bracketed in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-hashed-atpt (&optional beg end) 
"Relation of bracketed in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-hashed-atpt (&optional beg end) 
"Counts matches of lesserangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-hashed-until-point (&optional beg end) 
"Counts matches of lesserangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-hashed-atpt (&optional beg end) 
"Relation of lesserangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-hashed-atpt (&optional beg end) 
"Counts matches of greaterangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-hashed-until-point (&optional beg end) 
"Counts matches of greaterangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-hashed-atpt (&optional beg end) 
"Relation of greaterangled in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-hashed-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-hashed-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-hashed-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-hashed-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-hashed-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-hashed-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-hashed-atpt (&optional beg end) 
"Counts matches of parentized in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hashed beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-hashed-until-point (&optional beg end) 
"Counts matches of parentized in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-hashed-atpt (&optional beg end) 
"Relation of parentized in hashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hashed beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-hyphened-atpt (&optional beg end) 
"Counts matches of braced in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-hyphened-until-point (&optional beg end) 
"Counts matches of braced in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-hyphened-atpt (&optional beg end) 
"Relation of braced in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-hyphened-atpt (&optional beg end) 
"Counts matches of bracketed in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-hyphened-until-point (&optional beg end) 
"Counts matches of bracketed in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-hyphened-atpt (&optional beg end) 
"Relation of bracketed in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-hyphened-atpt (&optional beg end) 
"Counts matches of lesserangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-hyphened-until-point (&optional beg end) 
"Counts matches of lesserangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-hyphened-atpt (&optional beg end) 
"Relation of lesserangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-hyphened-atpt (&optional beg end) 
"Counts matches of greaterangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-hyphened-until-point (&optional beg end) 
"Counts matches of greaterangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-hyphened-atpt (&optional beg end) 
"Relation of greaterangled in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-hyphened-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-hyphened-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-hyphened-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-hyphened-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-hyphened-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-hyphened-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-hyphened-atpt (&optional beg end) 
"Counts matches of parentized in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hyphened beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-hyphened-until-point (&optional beg end) 
"Counts matches of parentized in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hyphened beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-hyphened-atpt (&optional beg end) 
"Relation of parentized in hyphened if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'hyphened beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-singlequoted-atpt (&optional beg end) 
"Counts matches of braced in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-singlequoted-until-point (&optional beg end) 
"Counts matches of braced in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-singlequoted-atpt (&optional beg end) 
"Relation of braced in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-singlequoted-atpt (&optional beg end) 
"Counts matches of bracketed in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-singlequoted-until-point (&optional beg end) 
"Counts matches of bracketed in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-singlequoted-atpt (&optional beg end) 
"Relation of bracketed in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-singlequoted-atpt (&optional beg end) 
"Counts matches of lesserangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-singlequoted-until-point (&optional beg end) 
"Counts matches of lesserangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-singlequoted-atpt (&optional beg end) 
"Relation of lesserangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-singlequoted-atpt (&optional beg end) 
"Counts matches of greaterangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-singlequoted-until-point (&optional beg end) 
"Counts matches of greaterangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-singlequoted-atpt (&optional beg end) 
"Relation of greaterangled in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-singlequoted-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-singlequoted-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-singlequoted-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-singlequoted-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-singlequoted-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-singlequoted-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-singlequoted-atpt (&optional beg end) 
"Counts matches of parentized in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'singlequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-singlequoted-until-point (&optional beg end) 
"Counts matches of parentized in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'singlequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-singlequoted-atpt (&optional beg end) 
"Relation of parentized in singlequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'singlequoted beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-slashed-atpt (&optional beg end) 
"Counts matches of braced in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-slashed-until-point (&optional beg end) 
"Counts matches of braced in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-slashed-atpt (&optional beg end) 
"Relation of braced in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-slashed-atpt (&optional beg end) 
"Counts matches of bracketed in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-slashed-until-point (&optional beg end) 
"Counts matches of bracketed in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-slashed-atpt (&optional beg end) 
"Relation of bracketed in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-slashed-atpt (&optional beg end) 
"Counts matches of lesserangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-slashed-until-point (&optional beg end) 
"Counts matches of lesserangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-slashed-atpt (&optional beg end) 
"Relation of lesserangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-slashed-atpt (&optional beg end) 
"Counts matches of greaterangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-slashed-until-point (&optional beg end) 
"Counts matches of greaterangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-slashed-atpt (&optional beg end) 
"Relation of greaterangled in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-slashed-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-slashed-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-slashed-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-slashed-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-slashed-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-slashed-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-slashed-atpt (&optional beg end) 
"Counts matches of parentized in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'slashed beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-slashed-until-point (&optional beg end) 
"Counts matches of parentized in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'slashed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-slashed-atpt (&optional beg end) 
"Relation of parentized in slashed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'slashed beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-stared-atpt (&optional beg end) 
"Counts matches of braced in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-stared-until-point (&optional beg end) 
"Counts matches of braced in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-stared-atpt (&optional beg end) 
"Relation of braced in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-stared-atpt (&optional beg end) 
"Counts matches of bracketed in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-stared-until-point (&optional beg end) 
"Counts matches of bracketed in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-stared-atpt (&optional beg end) 
"Relation of bracketed in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-stared-atpt (&optional beg end) 
"Counts matches of lesserangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-stared-until-point (&optional beg end) 
"Counts matches of lesserangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-stared-atpt (&optional beg end) 
"Relation of lesserangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-stared-atpt (&optional beg end) 
"Counts matches of greaterangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-stared-until-point (&optional beg end) 
"Counts matches of greaterangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-stared-atpt (&optional beg end) 
"Relation of greaterangled in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-stared-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-stared-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-stared-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-stared-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-stared-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-stared-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-stared-atpt (&optional beg end) 
"Counts matches of parentized in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'stared beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-stared-until-point (&optional beg end) 
"Counts matches of parentized in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'stared beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-stared-atpt (&optional beg end) 
"Relation of parentized in stared if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'stared beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-tilded-atpt (&optional beg end) 
"Counts matches of braced in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-tilded-until-point (&optional beg end) 
"Counts matches of braced in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-tilded-atpt (&optional beg end) 
"Relation of braced in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-tilded-atpt (&optional beg end) 
"Counts matches of bracketed in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-tilded-until-point (&optional beg end) 
"Counts matches of bracketed in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-tilded-atpt (&optional beg end) 
"Relation of bracketed in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-tilded-atpt (&optional beg end) 
"Counts matches of lesserangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-tilded-until-point (&optional beg end) 
"Counts matches of lesserangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-tilded-atpt (&optional beg end) 
"Relation of lesserangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-tilded-atpt (&optional beg end) 
"Counts matches of greaterangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-tilded-until-point (&optional beg end) 
"Counts matches of greaterangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-tilded-atpt (&optional beg end) 
"Relation of greaterangled in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-tilded-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-tilded-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-tilded-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-tilded-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-tilded-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-tilded-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-tilded-atpt (&optional beg end) 
"Counts matches of parentized in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'tilded beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-tilded-until-point (&optional beg end) 
"Counts matches of parentized in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'tilded beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-tilded-atpt (&optional beg end) 
"Relation of parentized in tilded if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'tilded beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-underscored-atpt (&optional beg end) 
"Counts matches of braced in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-underscored-until-point (&optional beg end) 
"Counts matches of braced in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-underscored-atpt (&optional beg end) 
"Relation of braced in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-underscored-atpt (&optional beg end) 
"Counts matches of bracketed in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-underscored-until-point (&optional beg end) 
"Counts matches of bracketed in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-underscored-atpt (&optional beg end) 
"Relation of bracketed in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-underscored-atpt (&optional beg end) 
"Counts matches of lesserangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-underscored-until-point (&optional beg end) 
"Counts matches of lesserangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-underscored-atpt (&optional beg end) 
"Relation of lesserangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-underscored-atpt (&optional beg end) 
"Counts matches of greaterangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-underscored-until-point (&optional beg end) 
"Counts matches of greaterangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-underscored-atpt (&optional beg end) 
"Relation of greaterangled in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-underscored-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-underscored-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-underscored-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-underscored-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-underscored-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-underscored-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-underscored-atpt (&optional beg end) 
"Counts matches of parentized in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'underscored beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-underscored-until-point (&optional beg end) 
"Counts matches of parentized in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'underscored beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-underscored-atpt (&optional beg end) 
"Relation of parentized in underscored if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'underscored beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-whitespaced-atpt (&optional beg end) 
"Counts matches of braced in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-whitespaced-until-point (&optional beg end) 
"Counts matches of braced in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-whitespaced-atpt (&optional beg end) 
"Relation of braced in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-whitespaced-atpt (&optional beg end) 
"Counts matches of bracketed in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-whitespaced-until-point (&optional beg end) 
"Counts matches of bracketed in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-whitespaced-atpt (&optional beg end) 
"Relation of bracketed in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-whitespaced-atpt (&optional beg end) 
"Counts matches of lesserangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-whitespaced-until-point (&optional beg end) 
"Counts matches of lesserangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-whitespaced-atpt (&optional beg end) 
"Relation of lesserangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-whitespaced-atpt (&optional beg end) 
"Counts matches of greaterangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-whitespaced-until-point (&optional beg end) 
"Counts matches of greaterangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-whitespaced-atpt (&optional beg end) 
"Relation of greaterangled in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-whitespaced-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-whitespaced-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-whitespaced-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-whitespaced-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-whitespaced-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-whitespaced-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'whitespaced beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-whitespaced-atpt (&optional beg end) 
"Counts matches of parentized in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'whitespaced beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-whitespaced-until-point (&optional beg end) 
"Counts matches of parentized in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'whitespaced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-whitespaced-atpt (&optional beg end) 
"Relation of parentized in whitespaced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'whitespaced beg end t (called-interactively-p 'any)))


;;; count/ratio ar-unpaired-delimited-passiv ar-paired-delimited-passiv end


;;; count/ratio ar-paired-delimited-passiv ar-unpaired-delimited-passiv  start

(defun ar-count-backslashed-in-braced-atpt (&optional beg end) 
"Counts matches of backslashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-braced-until-point (&optional beg end) 
"Counts matches of backslashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-braced-atpt (&optional beg end) 
"Relation of backslashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-braced-atpt (&optional beg end) 
"Counts matches of backticked in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-braced-until-point (&optional beg end) 
"Counts matches of backticked in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-braced-atpt (&optional beg end) 
"Relation of backticked in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-braced-atpt (&optional beg end) 
"Counts matches of coloned in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-braced-until-point (&optional beg end) 
"Counts matches of coloned in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-braced-atpt (&optional beg end) 
"Relation of coloned in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-braced-atpt (&optional beg end) 
"Counts matches of crossed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-braced-until-point (&optional beg end) 
"Counts matches of crossed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-braced-atpt (&optional beg end) 
"Relation of crossed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-braced-atpt (&optional beg end) 
"Counts matches of dollared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-braced-until-point (&optional beg end) 
"Counts matches of dollared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-braced-atpt (&optional beg end) 
"Relation of dollared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-braced-atpt (&optional beg end) 
"Counts matches of doublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-braced-until-point (&optional beg end) 
"Counts matches of doublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-braced-atpt (&optional beg end) 
"Relation of doublequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-braced-atpt (&optional beg end) 
"Counts matches of equalized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-braced-until-point (&optional beg end) 
"Counts matches of equalized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-braced-atpt (&optional beg end) 
"Relation of equalized in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-braced-atpt (&optional beg end) 
"Counts matches of hashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-braced-until-point (&optional beg end) 
"Counts matches of hashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-braced-atpt (&optional beg end) 
"Relation of hashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-braced-atpt (&optional beg end) 
"Counts matches of hyphened in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-braced-until-point (&optional beg end) 
"Counts matches of hyphened in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-braced-atpt (&optional beg end) 
"Relation of hyphened in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-braced-atpt (&optional beg end) 
"Counts matches of singlequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-braced-until-point (&optional beg end) 
"Counts matches of singlequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-braced-atpt (&optional beg end) 
"Relation of singlequoted in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-braced-atpt (&optional beg end) 
"Counts matches of slashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-braced-until-point (&optional beg end) 
"Counts matches of slashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-braced-atpt (&optional beg end) 
"Relation of slashed in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-braced-atpt (&optional beg end) 
"Counts matches of stared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-braced-until-point (&optional beg end) 
"Counts matches of stared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-braced-atpt (&optional beg end) 
"Relation of stared in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-braced-atpt (&optional beg end) 
"Counts matches of tilded in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-braced-until-point (&optional beg end) 
"Counts matches of tilded in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-braced-atpt (&optional beg end) 
"Relation of tilded in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-braced-atpt (&optional beg end) 
"Counts matches of underscored in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-braced-until-point (&optional beg end) 
"Counts matches of underscored in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-braced-atpt (&optional beg end) 
"Relation of underscored in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-braced-atpt (&optional beg end) 
"Counts matches of whitespaced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'braced beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-braced-until-point (&optional beg end) 
"Counts matches of whitespaced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'braced beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-braced-atpt (&optional beg end) 
"Relation of whitespaced in braced if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'braced beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-bracketed-atpt (&optional beg end) 
"Counts matches of backslashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-bracketed-until-point (&optional beg end) 
"Counts matches of backslashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-bracketed-atpt (&optional beg end) 
"Relation of backslashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-bracketed-atpt (&optional beg end) 
"Counts matches of backticked in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-bracketed-until-point (&optional beg end) 
"Counts matches of backticked in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-bracketed-atpt (&optional beg end) 
"Relation of backticked in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-bracketed-atpt (&optional beg end) 
"Counts matches of coloned in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-bracketed-until-point (&optional beg end) 
"Counts matches of coloned in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-bracketed-atpt (&optional beg end) 
"Relation of coloned in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-bracketed-atpt (&optional beg end) 
"Counts matches of crossed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-bracketed-until-point (&optional beg end) 
"Counts matches of crossed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-bracketed-atpt (&optional beg end) 
"Relation of crossed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-bracketed-atpt (&optional beg end) 
"Counts matches of dollared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-bracketed-until-point (&optional beg end) 
"Counts matches of dollared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-bracketed-atpt (&optional beg end) 
"Relation of dollared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-bracketed-atpt (&optional beg end) 
"Counts matches of doublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-bracketed-until-point (&optional beg end) 
"Counts matches of doublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-bracketed-atpt (&optional beg end) 
"Relation of doublequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-bracketed-atpt (&optional beg end) 
"Counts matches of equalized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-bracketed-until-point (&optional beg end) 
"Counts matches of equalized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-bracketed-atpt (&optional beg end) 
"Relation of equalized in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-bracketed-atpt (&optional beg end) 
"Counts matches of hashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-bracketed-until-point (&optional beg end) 
"Counts matches of hashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-bracketed-atpt (&optional beg end) 
"Relation of hashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-bracketed-atpt (&optional beg end) 
"Counts matches of hyphened in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-bracketed-until-point (&optional beg end) 
"Counts matches of hyphened in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-bracketed-atpt (&optional beg end) 
"Relation of hyphened in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-bracketed-atpt (&optional beg end) 
"Counts matches of singlequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-bracketed-until-point (&optional beg end) 
"Counts matches of singlequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-bracketed-atpt (&optional beg end) 
"Relation of singlequoted in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-bracketed-atpt (&optional beg end) 
"Counts matches of slashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-bracketed-until-point (&optional beg end) 
"Counts matches of slashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-bracketed-atpt (&optional beg end) 
"Relation of slashed in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-bracketed-atpt (&optional beg end) 
"Counts matches of stared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-bracketed-until-point (&optional beg end) 
"Counts matches of stared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-bracketed-atpt (&optional beg end) 
"Relation of stared in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-bracketed-atpt (&optional beg end) 
"Counts matches of tilded in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-bracketed-until-point (&optional beg end) 
"Counts matches of tilded in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-bracketed-atpt (&optional beg end) 
"Relation of tilded in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-bracketed-atpt (&optional beg end) 
"Counts matches of underscored in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-bracketed-until-point (&optional beg end) 
"Counts matches of underscored in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-bracketed-atpt (&optional beg end) 
"Relation of underscored in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-bracketed-atpt (&optional beg end) 
"Counts matches of whitespaced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'bracketed beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-bracketed-until-point (&optional beg end) 
"Counts matches of whitespaced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'bracketed beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-bracketed-atpt (&optional beg end) 
"Relation of whitespaced in bracketed if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'bracketed beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-lesserangled-atpt (&optional beg end) 
"Counts matches of backslashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-lesserangled-until-point (&optional beg end) 
"Counts matches of backslashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-lesserangled-atpt (&optional beg end) 
"Relation of backslashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-lesserangled-atpt (&optional beg end) 
"Counts matches of backticked in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-lesserangled-until-point (&optional beg end) 
"Counts matches of backticked in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-lesserangled-atpt (&optional beg end) 
"Relation of backticked in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-lesserangled-atpt (&optional beg end) 
"Counts matches of coloned in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-lesserangled-until-point (&optional beg end) 
"Counts matches of coloned in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-lesserangled-atpt (&optional beg end) 
"Relation of coloned in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-lesserangled-atpt (&optional beg end) 
"Counts matches of crossed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-lesserangled-until-point (&optional beg end) 
"Counts matches of crossed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-lesserangled-atpt (&optional beg end) 
"Relation of crossed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-lesserangled-atpt (&optional beg end) 
"Counts matches of dollared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-lesserangled-until-point (&optional beg end) 
"Counts matches of dollared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-lesserangled-atpt (&optional beg end) 
"Relation of dollared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-lesserangled-atpt (&optional beg end) 
"Counts matches of doublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-lesserangled-until-point (&optional beg end) 
"Counts matches of doublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-lesserangled-atpt (&optional beg end) 
"Relation of doublequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-lesserangled-atpt (&optional beg end) 
"Counts matches of equalized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-lesserangled-until-point (&optional beg end) 
"Counts matches of equalized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-lesserangled-atpt (&optional beg end) 
"Relation of equalized in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-lesserangled-atpt (&optional beg end) 
"Counts matches of hashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-lesserangled-until-point (&optional beg end) 
"Counts matches of hashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-lesserangled-atpt (&optional beg end) 
"Relation of hashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-lesserangled-atpt (&optional beg end) 
"Counts matches of hyphened in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-lesserangled-until-point (&optional beg end) 
"Counts matches of hyphened in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-lesserangled-atpt (&optional beg end) 
"Relation of hyphened in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-lesserangled-atpt (&optional beg end) 
"Counts matches of singlequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-lesserangled-until-point (&optional beg end) 
"Counts matches of singlequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-lesserangled-atpt (&optional beg end) 
"Relation of singlequoted in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-lesserangled-atpt (&optional beg end) 
"Counts matches of slashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-lesserangled-until-point (&optional beg end) 
"Counts matches of slashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-lesserangled-atpt (&optional beg end) 
"Relation of slashed in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-lesserangled-atpt (&optional beg end) 
"Counts matches of stared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-lesserangled-until-point (&optional beg end) 
"Counts matches of stared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-lesserangled-atpt (&optional beg end) 
"Relation of stared in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-lesserangled-atpt (&optional beg end) 
"Counts matches of tilded in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-lesserangled-until-point (&optional beg end) 
"Counts matches of tilded in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-lesserangled-atpt (&optional beg end) 
"Relation of tilded in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-lesserangled-atpt (&optional beg end) 
"Counts matches of underscored in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-lesserangled-until-point (&optional beg end) 
"Counts matches of underscored in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-lesserangled-atpt (&optional beg end) 
"Relation of underscored in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-lesserangled-atpt (&optional beg end) 
"Counts matches of whitespaced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'lesserangled beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-lesserangled-until-point (&optional beg end) 
"Counts matches of whitespaced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'lesserangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-lesserangled-atpt (&optional beg end) 
"Relation of whitespaced in lesserangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'lesserangled beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-greaterangled-atpt (&optional beg end) 
"Counts matches of backslashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-greaterangled-until-point (&optional beg end) 
"Counts matches of backslashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-greaterangled-atpt (&optional beg end) 
"Relation of backslashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-greaterangled-atpt (&optional beg end) 
"Counts matches of backticked in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-greaterangled-until-point (&optional beg end) 
"Counts matches of backticked in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-greaterangled-atpt (&optional beg end) 
"Relation of backticked in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-greaterangled-atpt (&optional beg end) 
"Counts matches of coloned in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-greaterangled-until-point (&optional beg end) 
"Counts matches of coloned in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-greaterangled-atpt (&optional beg end) 
"Relation of coloned in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-greaterangled-atpt (&optional beg end) 
"Counts matches of crossed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-greaterangled-until-point (&optional beg end) 
"Counts matches of crossed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-greaterangled-atpt (&optional beg end) 
"Relation of crossed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-greaterangled-atpt (&optional beg end) 
"Counts matches of dollared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-greaterangled-until-point (&optional beg end) 
"Counts matches of dollared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-greaterangled-atpt (&optional beg end) 
"Relation of dollared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-greaterangled-atpt (&optional beg end) 
"Counts matches of doublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-greaterangled-until-point (&optional beg end) 
"Counts matches of doublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-greaterangled-atpt (&optional beg end) 
"Relation of doublequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-greaterangled-atpt (&optional beg end) 
"Counts matches of equalized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-greaterangled-until-point (&optional beg end) 
"Counts matches of equalized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-greaterangled-atpt (&optional beg end) 
"Relation of equalized in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-greaterangled-atpt (&optional beg end) 
"Counts matches of hashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-greaterangled-until-point (&optional beg end) 
"Counts matches of hashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-greaterangled-atpt (&optional beg end) 
"Relation of hashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-greaterangled-atpt (&optional beg end) 
"Counts matches of hyphened in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-greaterangled-until-point (&optional beg end) 
"Counts matches of hyphened in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-greaterangled-atpt (&optional beg end) 
"Relation of hyphened in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-greaterangled-atpt (&optional beg end) 
"Counts matches of singlequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-greaterangled-until-point (&optional beg end) 
"Counts matches of singlequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-greaterangled-atpt (&optional beg end) 
"Relation of singlequoted in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-greaterangled-atpt (&optional beg end) 
"Counts matches of slashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-greaterangled-until-point (&optional beg end) 
"Counts matches of slashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-greaterangled-atpt (&optional beg end) 
"Relation of slashed in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-greaterangled-atpt (&optional beg end) 
"Counts matches of stared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-greaterangled-until-point (&optional beg end) 
"Counts matches of stared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-greaterangled-atpt (&optional beg end) 
"Relation of stared in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-greaterangled-atpt (&optional beg end) 
"Counts matches of tilded in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-greaterangled-until-point (&optional beg end) 
"Counts matches of tilded in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-greaterangled-atpt (&optional beg end) 
"Relation of tilded in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-greaterangled-atpt (&optional beg end) 
"Counts matches of underscored in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-greaterangled-until-point (&optional beg end) 
"Counts matches of underscored in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-greaterangled-atpt (&optional beg end) 
"Relation of underscored in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-greaterangled-atpt (&optional beg end) 
"Counts matches of whitespaced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'greaterangled beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-greaterangled-until-point (&optional beg end) 
"Counts matches of whitespaced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'greaterangled beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-greaterangled-atpt (&optional beg end) 
"Relation of whitespaced in greaterangled if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'greaterangled beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of backslashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of backslashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of backslashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of backticked in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of backticked in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of backticked in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of coloned in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of coloned in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of coloned in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of crossed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of crossed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of crossed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of dollared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of dollared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of dollared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of doublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of doublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of doublequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of equalized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of equalized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of equalized in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of hashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of hashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of hashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of hyphened in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of hyphened in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of hyphened in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of singlequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of singlequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of singlequoted in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of slashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of slashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of slashed in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of stared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of stared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of stared in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of tilded in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of tilded in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of tilded in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of underscored in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of underscored in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of underscored in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-leftrightsinglequoted-atpt (&optional beg end) 
"Counts matches of whitespaced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightsinglequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-leftrightsinglequoted-until-point (&optional beg end) 
"Counts matches of whitespaced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightsinglequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-leftrightsinglequoted-atpt (&optional beg end) 
"Relation of whitespaced in leftrightsinglequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightsinglequoted beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of backslashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of backslashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of backslashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of backticked in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of backticked in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of backticked in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of coloned in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of coloned in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of coloned in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of crossed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of crossed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of crossed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of dollared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of dollared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of dollared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of doublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of doublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of doublequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of equalized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of equalized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of equalized in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of hashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of hashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of hashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of hyphened in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of hyphened in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of hyphened in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of singlequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of singlequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of singlequoted in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of slashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of slashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of slashed in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of stared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of stared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of stared in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of tilded in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of tilded in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of tilded in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of underscored in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of underscored in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of underscored in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-leftrightdoublequoted-atpt (&optional beg end) 
"Counts matches of whitespaced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightdoublequoted beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-leftrightdoublequoted-until-point (&optional beg end) 
"Counts matches of whitespaced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightdoublequoted beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-leftrightdoublequoted-atpt (&optional beg end) 
"Relation of whitespaced in leftrightdoublequoted if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'leftrightdoublequoted beg end t (called-interactively-p 'any)))


(defun ar-count-backslashed-in-parentized-atpt (&optional beg end) 
"Counts matches of backslashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-backslashed-in-parentized-until-point (&optional beg end) 
"Counts matches of backslashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backslashed-in-parentized-atpt (&optional beg end) 
"Relation of backslashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backslashed 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-backticked-in-parentized-atpt (&optional beg end) 
"Counts matches of backticked in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-backticked-in-parentized-until-point (&optional beg end) 
"Counts matches of backticked in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-backticked-in-parentized-atpt (&optional beg end) 
"Relation of backticked in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'backticked 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-coloned-in-parentized-atpt (&optional beg end) 
"Counts matches of coloned in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-coloned-in-parentized-until-point (&optional beg end) 
"Counts matches of coloned in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-coloned-in-parentized-atpt (&optional beg end) 
"Relation of coloned in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'coloned 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-crossed-in-parentized-atpt (&optional beg end) 
"Counts matches of crossed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-crossed-in-parentized-until-point (&optional beg end) 
"Counts matches of crossed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-crossed-in-parentized-atpt (&optional beg end) 
"Relation of crossed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'crossed 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-dollared-in-parentized-atpt (&optional beg end) 
"Counts matches of dollared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-dollared-in-parentized-until-point (&optional beg end) 
"Counts matches of dollared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-dollared-in-parentized-atpt (&optional beg end) 
"Relation of dollared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'dollared 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-doublequoted-in-parentized-atpt (&optional beg end) 
"Counts matches of doublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-doublequoted-in-parentized-until-point (&optional beg end) 
"Counts matches of doublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-doublequoted-in-parentized-atpt (&optional beg end) 
"Relation of doublequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'doublequoted 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-equalized-in-parentized-atpt (&optional beg end) 
"Counts matches of equalized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-equalized-in-parentized-until-point (&optional beg end) 
"Counts matches of equalized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-equalized-in-parentized-atpt (&optional beg end) 
"Relation of equalized in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'equalized 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-hashed-in-parentized-atpt (&optional beg end) 
"Counts matches of hashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-hashed-in-parentized-until-point (&optional beg end) 
"Counts matches of hashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hashed-in-parentized-atpt (&optional beg end) 
"Relation of hashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hashed 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-hyphened-in-parentized-atpt (&optional beg end) 
"Counts matches of hyphened in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-hyphened-in-parentized-until-point (&optional beg end) 
"Counts matches of hyphened in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-hyphened-in-parentized-atpt (&optional beg end) 
"Relation of hyphened in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'hyphened 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-singlequoted-in-parentized-atpt (&optional beg end) 
"Counts matches of singlequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-singlequoted-in-parentized-until-point (&optional beg end) 
"Counts matches of singlequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-singlequoted-in-parentized-atpt (&optional beg end) 
"Relation of singlequoted in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'singlequoted 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-slashed-in-parentized-atpt (&optional beg end) 
"Counts matches of slashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-slashed-in-parentized-until-point (&optional beg end) 
"Counts matches of slashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-slashed-in-parentized-atpt (&optional beg end) 
"Relation of slashed in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'slashed 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-stared-in-parentized-atpt (&optional beg end) 
"Counts matches of stared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-stared-in-parentized-until-point (&optional beg end) 
"Counts matches of stared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-stared-in-parentized-atpt (&optional beg end) 
"Relation of stared in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'stared 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-tilded-in-parentized-atpt (&optional beg end) 
"Counts matches of tilded in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-tilded-in-parentized-until-point (&optional beg end) 
"Counts matches of tilded in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-tilded-in-parentized-atpt (&optional beg end) 
"Relation of tilded in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'tilded 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-underscored-in-parentized-atpt (&optional beg end) 
"Counts matches of underscored in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-underscored-in-parentized-until-point (&optional beg end) 
"Counts matches of underscored in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-underscored-in-parentized-atpt (&optional beg end) 
"Relation of underscored in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'underscored 'parentized beg end t (called-interactively-p 'any)))


(defun ar-count-whitespaced-in-parentized-atpt (&optional beg end) 
"Counts matches of whitespaced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'parentized beg end nil (called-interactively-p 'any)))

(defun ar-count-whitespaced-in-parentized-until-point (&optional beg end) 
"Counts matches of whitespaced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'parentized beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-whitespaced-in-parentized-atpt (&optional beg end) 
"Relation of whitespaced in parentized if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'whitespaced 'parentized beg end t (called-interactively-p 'any)))


;;; count/ratio ar-paired-delimited-passiv ar-unpaired-delimited-passiv end


;;; count/ratio ar-atpt-counts-list ar-paired-delimited-passiv start

(defun ar-count-braced-in-anglednonest-atpt (&optional beg end) 
"Counts matches of braced in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-anglednonest-until-point (&optional beg end) 
"Counts matches of braced in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-anglednonest-atpt (&optional beg end) 
"Relation of braced in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-anglednonest-atpt (&optional beg end) 
"Counts matches of bracketed in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-anglednonest-until-point (&optional beg end) 
"Counts matches of bracketed in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-anglednonest-atpt (&optional beg end) 
"Relation of bracketed in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-anglednonest-atpt (&optional beg end) 
"Counts matches of lesserangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-anglednonest-until-point (&optional beg end) 
"Counts matches of lesserangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-anglednonest-atpt (&optional beg end) 
"Relation of lesserangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-anglednonest-atpt (&optional beg end) 
"Counts matches of greaterangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-anglednonest-until-point (&optional beg end) 
"Counts matches of greaterangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-anglednonest-atpt (&optional beg end) 
"Relation of greaterangled in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-anglednonest-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-anglednonest-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-anglednonest-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-anglednonest-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-anglednonest-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-anglednonest-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-anglednonest-atpt (&optional beg end) 
"Counts matches of parentized in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'anglednonest beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-anglednonest-until-point (&optional beg end) 
"Counts matches of parentized in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'anglednonest beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-anglednonest-atpt (&optional beg end) 
"Relation of parentized in anglednonest if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'anglednonest beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of braced in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of braced in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-greateranglednested-atpt (&optional beg end) 
"Relation of braced in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of bracketed in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of bracketed in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-greateranglednested-atpt (&optional beg end) 
"Relation of bracketed in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of lesserangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of lesserangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-greateranglednested-atpt (&optional beg end) 
"Relation of lesserangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of greaterangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of greaterangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-greateranglednested-atpt (&optional beg end) 
"Relation of greaterangled in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-greateranglednested-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-greateranglednested-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-greateranglednested-atpt (&optional beg end) 
"Counts matches of parentized in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greateranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-greateranglednested-until-point (&optional beg end) 
"Counts matches of parentized in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greateranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-greateranglednested-atpt (&optional beg end) 
"Relation of parentized in greateranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'greateranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of braced in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of braced in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-lesseranglednested-atpt (&optional beg end) 
"Relation of braced in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of bracketed in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of bracketed in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-lesseranglednested-atpt (&optional beg end) 
"Relation of bracketed in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of lesserangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of lesserangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-lesseranglednested-atpt (&optional beg end) 
"Relation of lesserangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of greaterangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of greaterangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-lesseranglednested-atpt (&optional beg end) 
"Relation of greaterangled in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-lesseranglednested-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-lesseranglednested-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-lesseranglednested-atpt (&optional beg end) 
"Counts matches of parentized in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesseranglednested beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-lesseranglednested-until-point (&optional beg end) 
"Counts matches of parentized in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesseranglednested beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-lesseranglednested-atpt (&optional beg end) 
"Relation of parentized in lesseranglednested if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'lesseranglednested beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-csv-atpt (&optional beg end) 
"Counts matches of braced in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-csv-until-point (&optional beg end) 
"Counts matches of braced in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-csv-atpt (&optional beg end) 
"Relation of braced in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-csv-atpt (&optional beg end) 
"Counts matches of bracketed in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-csv-until-point (&optional beg end) 
"Counts matches of bracketed in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-csv-atpt (&optional beg end) 
"Relation of bracketed in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-csv-atpt (&optional beg end) 
"Counts matches of lesserangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-csv-until-point (&optional beg end) 
"Counts matches of lesserangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-csv-atpt (&optional beg end) 
"Relation of lesserangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-csv-atpt (&optional beg end) 
"Counts matches of greaterangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-csv-until-point (&optional beg end) 
"Counts matches of greaterangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-csv-atpt (&optional beg end) 
"Relation of greaterangled in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-csv-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-csv-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-csv-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-csv-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-csv-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-csv-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-csv-atpt (&optional beg end) 
"Counts matches of parentized in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'csv beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-csv-until-point (&optional beg end) 
"Counts matches of parentized in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'csv beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-csv-atpt (&optional beg end) 
"Relation of parentized in csv if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'csv beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-line-atpt (&optional beg end) 
"Counts matches of braced in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-line-until-point (&optional beg end) 
"Counts matches of braced in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-line-atpt (&optional beg end) 
"Relation of braced in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'line beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-line-atpt (&optional beg end) 
"Counts matches of bracketed in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-line-until-point (&optional beg end) 
"Counts matches of bracketed in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-line-atpt (&optional beg end) 
"Relation of bracketed in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'line beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-line-atpt (&optional beg end) 
"Counts matches of lesserangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-line-until-point (&optional beg end) 
"Counts matches of lesserangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-line-atpt (&optional beg end) 
"Relation of lesserangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'line beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-line-atpt (&optional beg end) 
"Counts matches of greaterangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-line-until-point (&optional beg end) 
"Counts matches of greaterangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-line-atpt (&optional beg end) 
"Relation of greaterangled in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'line beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-line-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-line-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-line-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'line beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-line-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-line-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-line-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'line beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-line-atpt (&optional beg end) 
"Counts matches of parentized in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'line beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-line-until-point (&optional beg end) 
"Counts matches of parentized in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'line beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-line-atpt (&optional beg end) 
"Relation of parentized in line if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'line beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-paragraph-atpt (&optional beg end) 
"Counts matches of braced in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-paragraph-until-point (&optional beg end) 
"Counts matches of braced in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-paragraph-atpt (&optional beg end) 
"Relation of braced in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-paragraph-atpt (&optional beg end) 
"Counts matches of bracketed in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-paragraph-until-point (&optional beg end) 
"Counts matches of bracketed in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-paragraph-atpt (&optional beg end) 
"Relation of bracketed in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-paragraph-atpt (&optional beg end) 
"Counts matches of lesserangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-paragraph-until-point (&optional beg end) 
"Counts matches of lesserangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-paragraph-atpt (&optional beg end) 
"Relation of lesserangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-paragraph-atpt (&optional beg end) 
"Counts matches of greaterangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-paragraph-until-point (&optional beg end) 
"Counts matches of greaterangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-paragraph-atpt (&optional beg end) 
"Relation of greaterangled in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-paragraph-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-paragraph-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-paragraph-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-paragraph-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-paragraph-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-paragraph-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-paragraph-atpt (&optional beg end) 
"Counts matches of parentized in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'paragraph beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-paragraph-until-point (&optional beg end) 
"Counts matches of parentized in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'paragraph beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-paragraph-atpt (&optional beg end) 
"Relation of parentized in paragraph if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'paragraph beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-region-atpt (&optional beg end) 
"Counts matches of braced in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-region-until-point (&optional beg end) 
"Counts matches of braced in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-region-atpt (&optional beg end) 
"Relation of braced in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'region beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-region-atpt (&optional beg end) 
"Counts matches of bracketed in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-region-until-point (&optional beg end) 
"Counts matches of bracketed in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-region-atpt (&optional beg end) 
"Relation of bracketed in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'region beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-region-atpt (&optional beg end) 
"Counts matches of lesserangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-region-until-point (&optional beg end) 
"Counts matches of lesserangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-region-atpt (&optional beg end) 
"Relation of lesserangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'region beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-region-atpt (&optional beg end) 
"Counts matches of greaterangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-region-until-point (&optional beg end) 
"Counts matches of greaterangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-region-atpt (&optional beg end) 
"Relation of greaterangled in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'region beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-region-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-region-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-region-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'region beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-region-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-region-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-region-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'region beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-region-atpt (&optional beg end) 
"Counts matches of parentized in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'region beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-region-until-point (&optional beg end) 
"Counts matches of parentized in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'region beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-region-atpt (&optional beg end) 
"Relation of parentized in region if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'region beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-sentence-atpt (&optional beg end) 
"Counts matches of braced in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-sentence-until-point (&optional beg end) 
"Counts matches of braced in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-sentence-atpt (&optional beg end) 
"Relation of braced in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-sentence-atpt (&optional beg end) 
"Counts matches of bracketed in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-sentence-until-point (&optional beg end) 
"Counts matches of bracketed in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-sentence-atpt (&optional beg end) 
"Relation of bracketed in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-sentence-atpt (&optional beg end) 
"Counts matches of lesserangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-sentence-until-point (&optional beg end) 
"Counts matches of lesserangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-sentence-atpt (&optional beg end) 
"Relation of lesserangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-sentence-atpt (&optional beg end) 
"Counts matches of greaterangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-sentence-until-point (&optional beg end) 
"Counts matches of greaterangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-sentence-atpt (&optional beg end) 
"Relation of greaterangled in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-sentence-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-sentence-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-sentence-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-sentence-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-sentence-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-sentence-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-sentence-atpt (&optional beg end) 
"Counts matches of parentized in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'sentence beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-sentence-until-point (&optional beg end) 
"Counts matches of parentized in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'sentence beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-sentence-atpt (&optional beg end) 
"Relation of parentized in sentence if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'sentence beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-string-atpt (&optional beg end) 
"Counts matches of braced in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-string-until-point (&optional beg end) 
"Counts matches of braced in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-string-atpt (&optional beg end) 
"Relation of braced in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'string beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-string-atpt (&optional beg end) 
"Counts matches of bracketed in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-string-until-point (&optional beg end) 
"Counts matches of bracketed in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-string-atpt (&optional beg end) 
"Relation of bracketed in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'string beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-string-atpt (&optional beg end) 
"Counts matches of lesserangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-string-until-point (&optional beg end) 
"Counts matches of lesserangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-string-atpt (&optional beg end) 
"Relation of lesserangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'string beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-string-atpt (&optional beg end) 
"Counts matches of greaterangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-string-until-point (&optional beg end) 
"Counts matches of greaterangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-string-atpt (&optional beg end) 
"Relation of greaterangled in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'string beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-string-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-string-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-string-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'string beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-string-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-string-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-string-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'string beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-string-atpt (&optional beg end) 
"Counts matches of parentized in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'string beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-string-until-point (&optional beg end) 
"Counts matches of parentized in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'string beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-string-atpt (&optional beg end) 
"Relation of parentized in string if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'string beg end t (called-interactively-p 'any)))


(defun ar-count-braced-in-buffer-atpt (&optional beg end) 
"Counts matches of braced in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-braced-in-buffer-until-point (&optional beg end) 
"Counts matches of braced in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-braced-in-buffer-atpt (&optional beg end) 
"Relation of braced in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'braced 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-bracketed-in-buffer-atpt (&optional beg end) 
"Counts matches of bracketed in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-bracketed-in-buffer-until-point (&optional beg end) 
"Counts matches of bracketed in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-bracketed-in-buffer-atpt (&optional beg end) 
"Relation of bracketed in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'bracketed 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-lesserangled-in-buffer-atpt (&optional beg end) 
"Counts matches of lesserangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-lesserangled-in-buffer-until-point (&optional beg end) 
"Counts matches of lesserangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-lesserangled-in-buffer-atpt (&optional beg end) 
"Relation of lesserangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'lesserangled 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-greaterangled-in-buffer-atpt (&optional beg end) 
"Counts matches of greaterangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-greaterangled-in-buffer-until-point (&optional beg end) 
"Counts matches of greaterangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-greaterangled-in-buffer-atpt (&optional beg end) 
"Relation of greaterangled in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'greaterangled 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightsinglequoted-in-buffer-atpt (&optional beg end) 
"Counts matches of leftrightsinglequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightsinglequoted-in-buffer-until-point (&optional beg end) 
"Counts matches of leftrightsinglequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightsinglequoted-in-buffer-atpt (&optional beg end) 
"Relation of leftrightsinglequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightsinglequoted 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-leftrightdoublequoted-in-buffer-atpt (&optional beg end) 
"Counts matches of leftrightdoublequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-leftrightdoublequoted-in-buffer-until-point (&optional beg end) 
"Counts matches of leftrightdoublequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-leftrightdoublequoted-in-buffer-atpt (&optional beg end) 
"Relation of leftrightdoublequoted in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'leftrightdoublequoted 'buffer beg end t (called-interactively-p 'any)))


(defun ar-count-parentized-in-buffer-atpt (&optional beg end) 
"Counts matches of parentized in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'buffer beg end nil (called-interactively-p 'any)))

(defun ar-count-parentized-in-buffer-until-point (&optional beg end) 
"Counts matches of parentized in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'buffer beg (point) nil (called-interactively-p 'any)))

(defun ar-ratio-parentized-in-buffer-atpt (&optional beg end) 
"Relation of parentized in buffer if useful, returns nil otherwise "
  (interactive)
  (ar-th-ratio-base 'parentized 'buffer beg end t (called-interactively-p 'any)))


;;; count/ratio  ar-atpt-counts-list ar-paired-delimited-passiv end




(provide 'thingatpt-count-ratio)
;;; thingatpt-count-ratio.el ends here
