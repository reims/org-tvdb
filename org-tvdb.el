;;; org-tvdb.el --- track tv shows with orgmode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Reimer Backhaus

;; Author: Reimer Backhaus <rbackhaus@gmail.com>
;; Package-Requires: ((request-deferred "20160419.1605") (s "20180406.108"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Track tv shows with orgmode.

;;; Code:

(require 's)
(require 'deferred)
(require 'request-deferred)

(defcustom org-tvdb-api-key nil
  "API key for tvdb.org."
  :type 'string)

(defcustom org-tvdb-released-status "RELEASED"
  "Todo status for released but unwatched episodes."
  :type 'string)

(defcustom org-tvdb-unreleased-status "UNRELEASED"
  "Todo status for unreleased episodes."
  :type 'string)

(defcustom org-tvdb-watched-status "WATCHED"
  "Todo status for watched episodes."
  :type 'string)

(defvar org-tvdb--token nil)

;;;###autoload
(defun org-tvdb-insert-todo-list ()
  "Insert todo list of the form `org-tvdb-unreleased-status' `org-tvdb-released-status' | `org-tvdb-watched-status'."
  (interactive)
  (backward-page)
  (insert (format "#+TODO: %s(%c) %s(%c) | %s(%c)"
		  org-tvdb-unreleased-status
		  (downcase (elt org-tvdb-unreleased-status 0))
		  org-tvdb-released-status
		  (downcase (elt org-tvdb-released-status 0))
		  org-tvdb-watched-status
		  (downcase (elt org-tvdb-watched-status 0))))
  (org-ctrl-c-ctrl-c)
  (org-return))

(defun org-tvdb--login ()
  "POST `org-tvdb-api-key' to the login endpoint to receive token."
  (deferred:$
    (request-deferred "https://api.thetvdb.com/login"
		      :type "POST"
		      :headers '(("Content-Type" . "application/json"))
		      :parser 'json-read
		      :data (json-encode `(("apikey" . ,org-tvdb-api-key))))
    (deferred:nextc it
      (lambda (response)
	(if (not (= 200 (request-response-status-code response)))
	    (error "Failed to login")
	  (setq org-tvdb--token (alist-get 'token (request-response-data response))))))))

(defun org-tvdb--head-request (end-point &optional params)
  "Wrap the `request-deferred' call to make a HEAD http call to END-POINT with PARAMS."
  (let ((url (s-concat "https://api.thetvdb.com/" end-point)))
    (request-deferred url
		      :type "HEAD"
		      :parser 'json-read
		      :headers `(("Authorization" . ,(s-concat "Bearer " org-tvdb--token)))
		      :params params)))

(defun org-tvdb--query-request (end-point &optional params)
  "Wrap the `request-deferred' call to query END-POINT with PARAMS."
  (let ((url (s-concat "https://api.thetvdb.com/" end-point)))
    (request-deferred url
		      :parser 'json-read
		      :headers `(("Authorization" . ,(s-concat "Bearer " org-tvdb--token)))
		      :params params)))

(defun org-tvdb--query (query-fn finalize-fn end-point &optional params)
  "Query END-POINT with PARAMS and login if necessary.

The query is done with QUERY-FN.  QUERY-FN must return a deferred
object that returns a response.  The result of calling
FINALIZE-FN on the response is returned."
  (deferred:$
    (funcall query-fn end-point params)
    (deferred:nextc it
      (lambda (response)
	(case (request-response-status-code response)
	  (200 (deferred:succeed (funcall finalize-fn response)))
	  (401 (deferred:$
		 (org-tvdb--login)
		 (deferred:nextc it
		   (lambda (_)
		     (funcall query-fn end-point params)))
		 (deferred:nextc it
		   (lambda (response)
		     (if (= 200 (request-response-status-code response))
			 (deferred:succeed (funcall finalize-fn response))
		       (error "Query failed")))))))))))

(defun org-tvdb--time-to-millis (time)
  "Convert TIME into milliseconds since epoch."
  (truncate (* 1000 (float-time time))))

(defun org-tvdb--current-millis ()
  "Get current millis since epoch."
  (org-tvdb--time-to-millis (current-time)))

(defun org-tvdb--move-to-series ()
  "Move to the first parent that has a TVDBID property."
  (let ((id (org-entry-get (point) "TVDBID")))
    (while (and (> (org-current-level) 1) (null id))
      (outline-up-heading 1 t)
      (setq id (org-entry-get (point) "TVDBID")))
    (or	id
	(error "Not in series"))))

(defun org-tvdb--move-to-season ()
  "Move to the first parent that has a TVDB_AIRED_SEASON property."
  (let ((aired-season (org-entry-get (point) "TVDB_AIRED_SEASON")))
    (while (and (> (org-current-level) 1) (null aired-season))
      (outline-up-heading 1 t)
      (setq aired-season (org-entry-get (point) "TVDB_AIRED_SEASON")))
    (or aired-season
	(error "Not in season"))))

(parse-time-string "2017-01-01")

(defun org-tvdb--has-been-released? (episode)
  "Return non-nil if first aired date of EPISODE is in the past."
  (destructuring-bind (_ _ _ current-day current-month current-year _ _ _) (decode-time (current-time))
    (destructuring-bind (_ _ _ release-day release-month release-year _ _ _) (parse-time-string (alist-get 'firstAired episode))
      (and (>= current-year release-year)
	   (>= current-month release-month)
	   (>= current-day release-day)))))

(defun org-tvdb--insert-episode (episode)
  "Insert episode defined by the alist EPISODE."
  (if (= 1 (alist-get 'airedEpisodeNumber episode))
      (org-insert-subheading nil)
    (org-insert-heading))
  (insert (alist-get 'episodeName episode))
  (if (org-tvdb--has-been-released? episode)
      (org-todo org-tvdb-released-status)
    (org-todo org-tvdb-unreleased-status))
  (org-return t)
  (insert (alist-get 'overview episode))
  (fill-paragraph)
  (org-entry-put (point) "TVDB_EPISODEID" (number-to-string (alist-get 'id episode)))
  (org-entry-put (point) "TVDB_LAST_UPDATE" (number-to-string (alist-get 'lastUpdated episode)))
  (org-entry-put (point) "TVDB_FIRST_AIRED" (alist-get 'firstAired episode))
  (org-entry-put (point) "TVDB_NUMBER_IN_SEASON" (number-to-string (alist-get 'airedEpisodeNumber episode)))
  (outline-end-of-subtree))

(defun org-tvdb--insert-season (season data)
  "Insert season number SEASON with episodes from DATA."
  (let ((season-level (+ 1 (org-current-level))))
    (outline-end-of-subtree)
    (org-return)
    (org-insert-heading)
    (insert "Season " (number-to-string season))
    (let ((diff (- (org-current-level) season-level)))
      (cond
       ((> diff 0) (dotimes (_ diff)
		     (org-promote-subtree)))
       ((< diff 0) (dotimes (_ (* -1 diff))
		     (org-demote-subtree)))))
    (org-entry-put (point) "TVDB_AIRED_SEASON" (number-to-string season))
    (org-return t))
  (let ((episodes (alist-get 'data data)))
    (dotimes (idx (length episodes))
      (org-tvdb--insert-episode (elt episodes idx)))
    (outline-up-heading 1)
    (outline-toggle-children)
    (outline-toggle-children)))

(defun org-tvdb--fetch-season (series-id season)
  "Fetch the episodes for season number SEASON from series SERIES-ID."
  (let ((end-point (s-concat "series/" series-id "/episodes/query")))
    (org-tvdb--query #'org-tvdb--query-request #'request-response-data end-point `(("airedSeason" . ,season)))))

(defun org-tvdb--fetch-series (series-id)
  "Fetch series with id SERIES-ID."
  (let ((end-point (s-concat "series/" series-id)))
    (org-tvdb--query #'org-tvdb--query-request #'request-response-data end-point)))

(defun org-tvdb--fetch-series-head (series-id)
  "Fetch series HEAD with id SERIES-ID."
  (let ((end-point (s-concat "series/" series-id)))
    (message "end-point %s" end-point)
    (org-tvdb--query #'org-tvdb--query-request #'identity end-point)))

(defun org-tvdb--fetch-series-summary (series-id)
  "Fetch summary for series with SERIES-ID."
  (org-tvdb--query #'org-tvdb--query-request #'request-response-data (s-concat "series/" series-id "/episodes/summary")))

(defun org-tvdb--search-series (name)
  "Search for series with name NAME."
  (let ((end-point "search/series"))
    (org-tvdb--query #'org-tvdb--query-request #'request-response-data end-point `(("name" . ,name)))))

(defun org-tvdb--insert-series-header (series)
  "Insert header for series SERIES."
  (org-insert-heading)
  (insert (alist-get 'seriesName series))
  (org-entry-put (point) "TVDBID" (number-to-string (alist-get 'id series)))
  (org-entry-put (point) "LAST_UPDATE" (number-to-string (org-tvdb--current-millis))))

;;;###autoload
(defun org-tvdb-add-series (name)
  "Add series with name NAME."
  (interactive "sName: ")
  (deferred:$
    (org-tvdb--search-series name)
    (deferred:nextc it
      (lambda (data)
	(let ((results (alist-get 'data data)))
	  (if (zerop (length results))
	      (error "No such series")
	    (org-tvdb--insert-series-header (elt results 0))))))))

(defun org-tvdb-add-season (season)
  "Add season number SEASON of current series."
  (interactive "nSeason: ")
  (let ((id (org-tvdb--move-to-series)))
    (deferred:$
      (org-tvdb--fetch-season id season)
      (deferred:nextc it
	(lambda (data)
	  (org-tvdb--insert-season season data))))))

(defun org-tvdb--must-update? (head-response)
  "Check if LAST_UPDATE property is earlier then last-modified header in HEAD-RESPONSE."
  (let ((last-updated-in-db (org-tvdb--time-to-millis (date-to-time (request-response-header head-response "last-modified"))))
	(last-updated-here (string-to-number (org-entry-get (point) "LAST_UPDATE"))))
    (> last-updated-in-db last-updated-here)))

;;;###autoload
(defun org-tvdb-update-series ()
  "Update current series."
  (interactive)
  (let ((id (org-tvdb--move-to-series)))
    (deferred:$
      (org-tvdb--fetch-series-head id)
      (deferred:nextc it
	(lambda (resp)
	  (if (org-tvdb--must-update? resp)
	      (org-tvdb--fetch-series-summary id)
	    (deferred:cancel it))))
      (deferred:nextc it
	(lambda (summary)
	  (message "%S" summary)
	  (outline-end-of-subtree)
	  (let* ((aired-season (string-to-number (org-tvdb--move-to-season)))
		 (seasons (map 'array #'string-to-number (alist-get 'airedSeasons (alist-get 'data summary))))
		 ;; (existing-seasons (remove-if (lambda (x)
		 ;; 				(or (= x 0) (> x aired-season)))
		 ;; 			      seasons))
		 (new-seasons (remove-if (lambda (x)
					   (or (= x 0) (<= x aired-season)))
					 seasons)))
	    (if (= 0 (length new-seasons))
		(message "no new seasons")
	      (progn
		(message "fetching season 0")
		(let ((idx 0))
		  (deferred:nextc (org-tvdb-add-season (elt new-seasons idx))
		    (deferred:lambda (_)
		      (when (< (incf idx) (length new-seasons))
			(message "fetching season %d" (elt new-seasons idx))
			(deferred:nextc (org-tvdb-add-season (elt new-seasons idx))
			  self)))))))))))))

;;;###autoload
(defun org-tvdb-mark-season-watched ()
  "Mark change status of all episodes in current season to `org-tvdb-watched-status'."
  (interactive)
  (when (org-entry-get (point) "TVDB_EPISODEID")
    (outline-up-heading 1))
  (outline-end-of-subtree)
  (while (outline-get-last-sibling)
    (org-todo org-tvdb-watched-status)))

;;;###autoload
(defun org-tvdb-mark-series-watched ()
  "Mark change status of all episodes in current series to `org-tvdb-watched-status'."
  (interactive)
  (org-tvdb--move-to-series)
  (outline-end-of-subtree)
  (if (org-entry-get (point) "TVDBID")
      (error "Empty series"))
  (while (outline-get-last-sibling)
    (org-tvdb-mark-season-watched)))

(provide 'org-tvdb)
;;; org-tvdb.el ends here
