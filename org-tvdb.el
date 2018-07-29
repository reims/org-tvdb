;;; org-tvdb.el --- track tv shows with orgmode                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Reimer Backhaus

;; Author: Reimer Backhaus <rbackhaus@gmail.com>
;; Package-Requires: ((request-deferred "0.2.0") (s "1.12.0") (emacs "25"))
;; Package-Version: 0.1.0
;; Homepage: https://github.com/reims/org-tvdb

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
(require 'cl-lib)

(defgroup org-tvdb nil
  "Customization group for the org-tvdb package."
  :group 'outlines)

(defcustom org-tvdb-api-key ""
  "API key for tvdb.org."
  :type 'string
  :group 'org-tvdb)

(defcustom org-tvdb-released-status "RELEASED"
  "Todo status for released but unwatched episodes."
  :type 'string
  :group 'org-tvdb)

(defcustom org-tvdb-unreleased-status "UNRELEASED"
  "Todo status for unreleased episodes."
  :type 'string
  :group 'org-tvdb)

(defcustom org-tvdb-watched-status "WATCHED"
  "Todo status for watched episodes."
  :type 'string
  :group 'org-tvdb)

(defcustom org-tvdb-agenda-command
  '("e" "Unwatched episodes" todo "RELEASED"
    ((org-agenda-files '("path/to/org-file/with/tvseries"))))
  "Custom agenda commands to show released but not watched episdes."
  :type 'sexp
  :group 'org-tvdb)

(defvar org-tvdb--token nil)

;;;###autoload
(defun org-tvdb-released-tree ()
  "Create sparse tree of all released but not watched episodes."
  (interactive)
  (org-occur (s-concat "^\\*+ " org-tvdb-released-status)))

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

(defun org-tvdb--query (finalize-fn end-point &optional params)
  "Query END-POINT with PARAMS and login if necessary.

The query is done with QUERY-FN.  QUERY-FN must return a deferred
object that returns a response.  The result of calling
FINALIZE-FN on the response is returned."
  (deferred:$
    (org-tvdb--query-request end-point params)
    (deferred:nextc it
      (lambda (response)
	(case (request-response-status-code response)
	  (200 (deferred:succeed (funcall finalize-fn response)))
	  (401 (deferred:$
		 (org-tvdb--login)
		 (deferred:nextc it
		   (lambda (_)
		     (org-tvdb--query-request end-point params)))
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
  (let ((id (org-entry-get (point) "TVDB_SERIES_ID")))
    (while (and (> (org-current-level) 1) (null id))
      (outline-up-heading 1 t)
      (setq id (org-entry-get (point) "TVDB_SERIES_ID")))
    (or	id
	(error "Not in series"))))

(defun org-tvdb--move-to-season ()
  "Move to the first parent that has a TVDB_AIRED_SEASON property."
  (outline-end-of-subtree)
  (let ((aired-season (or (org-entry-get (point) "TVDB_AIRED_SEASON")
			  (when (org-entry-get (point) "TVDB_SERIES_ID")
			    "0"))))
    (while (and (> (org-current-level) 1) (null aired-season))
      (outline-up-heading 1 t)
      (setq aired-season (org-entry-get (point) "TVDB_AIRED_SEASON"))
      (when (org-entry-get (point) "TVDB_SERIES_ID")
	(setq aired-season "0")))
    (or aired-season
	(error "Not in series"))))

(defun org-tvdb--unreleased-p (episode)
  "Return non-nil if first aired date of EPISODE is in the past."
  (let ((firstAired (alist-get 'firstAired episode)))
    (or (or (null firstAired) (zerop (length firstAired)))
	(destructuring-bind (_ _ _ current-day current-month current-year _ _ _) (decode-time (current-time))
	  (destructuring-bind (_ _ _ release-day release-month release-year _ _ _) (parse-time-string firstAired)
	    (cond
	     ((> release-year current-year) t)
	     ((< release-year current-year) nil)
	     ((> release-month current-month) t)
	     ((< release-month current-month) nil)
	     ((> release-day current-day) t)))))))

(defun org-tvdb--heading-for-episode (episode)
  "Return name of episode or a generic name for EPISODE."
  (or (alist-get 'episodeName episode)
      (s-concat "Episode "
		(number-to-string (alist-get 'airedEpisodeNumber episode)))))

(defun org-tvdb--insert-episode (episode)
  "Insert episode defined by the alist EPISODE."
  (if (= 1 (alist-get 'airedEpisodeNumber episode))
      (org-insert-subheading nil)
    (org-insert-heading))
  (insert (org-tvdb--heading-for-episode episode))
  (if (org-tvdb--unreleased-p episode)
      (org-todo org-tvdb-unreleased-status)
    (org-todo org-tvdb-released-status))
  (org-return t)
  (when-let ((overview (alist-get 'overview episode)))
    (insert overview))
  (fill-paragraph)
  (org-entry-put (point) "TVDB_EPISODE_ID" (number-to-string (alist-get 'id episode)))
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
    (outline-end-of-subtree)
    (org-return t))
  (let ((episodes (alist-get 'data data)))
    (dotimes (idx (length episodes))
      (org-tvdb--insert-episode (elt episodes idx)))
    (outline-up-heading 1)
    (outline-toggle-children)
    (outline-toggle-children)))

(defun org-tvdb--fetch-season (series-id season)
  "Fetch the episodes for season number SEASON from series SERIES-ID."
  (let ((end-point (s-concat "series/"
			     (if (numberp series-id)
				 (number-to-string series-id)
			       series-id)
			     "/episodes/query")))
    (org-tvdb--query #'request-response-data
		     end-point
		     `(("airedSeason" . ,season)))))

(defun org-tvdb--fetch-series (series-id)
  "Fetch series with id SERIES-ID."
  (let ((end-point (s-concat "series/"
			     (if (numberp series-id)
				 (number-to-string series-id)
			       series-id))))
    (org-tvdb--query #'request-response-data
		     end-point)))

(defun org-tvdb--fetch-series-head (series-id)
  "Fetch series HEAD with id SERIES-ID."
  (let ((end-point (s-concat "series/"
			     (if (numberp series-id)
				 (number-to-string series-id)
			       series-id))))
    (org-tvdb--query #'identity
		     end-point)))

(defun org-tvdb--fetch-series-summary (series-id)
  "Fetch summary for series with SERIES-ID."
  (let ((end-point (s-concat "series/"
			     (if (numberp series-id)
				 (number-to-string series-id)
			       series-id)
			     "/episodes/summary")))
    (org-tvdb--query #'request-response-data
		     end-point)))

(defun org-tvdb--search-series (name)
  "Search for series with name NAME."
  (let ((end-point "search/series"))
    (org-tvdb--query #'request-response-data
		     end-point
		     `(("name" . ,name)))))

(defun org-tvdb--insert-series-header (series at-top-level)
  "Insert header for series SERIES.

If AT-TOP-LEVEL is non-nil, insert heading at top level."
  (org-insert-heading nil t at-top-level)
  (insert (alist-get 'seriesName series))
  (org-entry-put (point) "TVDB_SERIES_ID" (number-to-string (alist-get 'id series)))
  (org-entry-put (point) "LAST_UPDATE" (number-to-string (org-tvdb--current-millis))))

;;;###autoload
(defun org-tvdb-add-season (season)
  "Add season number SEASON of current series."
  (interactive "nSeason: ")
  (let ((id (org-tvdb--move-to-series)))
    (deferred:$
      (org-tvdb--fetch-season id season)
      (deferred:nextc it
	(lambda (data)
	  (org-tvdb--insert-season season data))))))

(defun org-tvdb--add-seasons (seasons)
  "Fetch all seasons in SEASONS from series at point."
  (unless (zerop (length seasons))
    (let ((idx 0))
      (deferred:nextc (org-tvdb-add-season (elt seasons idx))
	(deferred:lambda (_)
	  (when (< (incf idx) (length seasons))
	    (deferred:nextc (org-tvdb-add-season (elt seasons idx))
	      self)))))))

;;;###autoload
(defun org-tvdb-add-series (name &optional arg)
  "Add series with name NAME.

If ARG is non-nil (e.g. if called with \\[universal-argument]),
inserts series as top level heading."
  (interactive "sName: \nP")
  (deferred:$
    (org-tvdb--search-series name)
    (deferred:nextc it
      (lambda (data)
	(let ((results (alist-get 'data data)))
	  (if (zerop (length results))
	      (error "No such series")
	    (let* ((series (elt results 0))
		   (id (alist-get 'id series)))
	      (org-tvdb--insert-series-header series arg)
	      (org-tvdb--fetch-series-summary id))))))
    (deferred:nextc it
      (lambda (summary)
	(let ((seasons (sort (cl-remove-if #'zerop
					   (map 'array #'string-to-number
						(alist-get 'airedSeasons (alist-get 'data summary))))
			     #'<)))
	  (org-tvdb--add-seasons seasons))))))

(defun org-tvdb--update-episode (episode)
  "Update episode at point with data from EPISODE."
  (when (and (string-equal (org-get-todo-state) org-tvdb-unreleased-status)
	     (not (org-tvdb--unreleased-p episode)))
    (org-todo org-tvdb-released-status))
  (org-edit-headline (org-tvdb--heading-for-episode episode)))

(defun org-tvdb--update-episodes (episodes)
  "Update episodes in season at point with data from EPISODES

Expects to be at the last episode to update."
  (while (outline-get-last-sibling)
    (when-let* ((aired-episode-property (org-entry-get (point) "TVDB_NUMBER_IN_SEASON"))
		(aired-episode (string-to-number aired-episode-property))
		(episode (cl-find-if (lambda (ep)
				       (= aired-episode
					  (alist-get 'airedEpisodeNumber ep)))
				     episodes)))
      (org-tvdb--update-episode episode))))

;;;###autoload
(defun org-tvdb-update-season ()
  "Update season at point."
  (interactive)
  (let ((season (string-to-number (or (org-tvdb--move-to-season)
				      (error "Not in season"))))
	(series (string-to-number (save-excursion (org-entry-get (point) "TVDB_SERIES_ID" t)))))
    (if (zerop season)
	(error "Not in season")
      (deferred:$
	(org-tvdb--fetch-season series season)
	(deferred:nextc it
	  (lambda (season)
	    (outline-end-of-subtree)
	    (let* ((last-episode (string-to-number (or (save-excursion (org-entry-get (point) "TVDB_NUMBER_IN_SEASON"))
						       "0")))
		   (episodes (sort (alist-get 'data season)
				   (lambda (ep1 ep2)
				     (< (alist-get 'airedEpisodeNumber ep1)
					(alist-get 'airedEpisodeNumber ep2)))))
		   (new-episodes (cl-remove-if (lambda (ep)
						 (<= (alist-get 'airedEpisodeNumber ep) last-episode))
					       episodes)))
	      (when (< 0 last-episode)
		(org-tvdb--update-episodes episodes))
	      (seq-do #'org-tvdb--insert-episode new-episodes))))))))

(defun org-tvdb--must-update-p (head-response)
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
	  (if (org-tvdb--must-update-p resp)
	      (org-tvdb--fetch-series-summary id)
	    (deferred:cancel it))))
      (deferred:nextc it
	(lambda (summary)
	  (outline-end-of-subtree)
	  (let* ((aired-season (string-to-number (org-tvdb--move-to-season)))
		 (seasons (sort (map 'array #'string-to-number (alist-get 'airedSeasons (alist-get 'data summary)))
				#'<))
		 (new-seasons (cl-remove-if (lambda (x)
					      (or (= x 0) (<= x aired-season)))
					    seasons)))
	    (deferred:nextc
	      (org-tvdb-update-season)
	      (lambda (_)
		(org-tvdb--add-seasons new-seasons)))))))))

;;;###autoload
(defun org-tvdb-mark-season-watched ()
  "Mark change status of all episodes in current season to `org-tvdb-watched-status'."
  (interactive)
  (when (org-entry-get (point) "TVDB_EPISODE_ID")
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
  (if (org-entry-get (point) "TVDB_SERIES_ID")
      (error "Empty series"))
  (while (outline-get-last-sibling)
    (org-tvdb-mark-season-watched)))

(provide 'org-tvdb)
;;; org-tvdb.el ends here
