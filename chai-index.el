;;; chai-index.el --- Lexical index for the Chai Library -*- lexical-binding: t; -*-

;; Author: Yibie <yibie@outlook.com>
;; Keywords: outlines, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Builds a local SQLite index over the Chai Library so that `chai-search'
;; can find passages across every book.
;;
;; The index is a pure side-car: it never modifies the source Org files, and
;; deleting `chai-index-file' costs nothing but a rebuild.
;;
;; Two design points are worth stating up front, because everything else
;; follows from them:
;;
;; 1. A chunk stores the *raw* buffer region (`beg_pos' / `end_pos') together
;;    with a *cleaned* copy of that region's text.  Jumping uses the region;
;;    searching and display use the cleaned text.  Because the cleaned text is
;;    never mapped back onto the original, the whole class of offset-drift bugs
;;    disappears instead of being defended against.
;;
;; 2. FTS5's `unicode61' tokenizer does not segment CJK: a run of Chinese is
;;    swallowed as a single token, so searching for a word inside it fails.
;;    Chai therefore indexes overlapping CJK bigrams and segments the query the
;;    same way.  See `chai-index--tokens'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'sqlite)
(require 'chai)
(require 'chai-library)

;;; Customization

(defgroup chai-index nil
  "Lexical index for the Chai Library."
  :group 'chai)

(defcustom chai-index-file
  (expand-file-name "index.db" (expand-file-name "chai/" user-emacs-directory))
  "Path of the SQLite database holding the Chai search index.
The file is derived data: deleting it only costs a rebuild."
  :type 'file
  :group 'chai-index)

(defcustom chai-index-preview-chars 240
  "How much of a passage the index stores for display.

The results buffer shows a few lines of a passage, not the whole thing, so
storing every passage in full is the single largest item in the index — over
half of it on a real library.  What is stored is only the preview; the full
passage is read back from its source file on demand by
`chai-search-passage-text', which is exact even after the file has been
edited.  Raising this makes results richer and the index proportionally
larger."
  :type 'integer
  :group 'chai-index)

(defcustom chai-curl-program "curl"
  "Program used to reach a local model server.

Curl rather than Emacs's own URL library because these requests go to a server
on this machine: a system-wide proxy would otherwise be applied to `localhost'
and answer with a gateway error."
  :type 'string
  :group 'chai-index)

(defcustom chai-index-chunk-max-chars 800
  "Soft upper bound on the cleaned length of one chunk.
Consecutive paragraphs are merged until adding the next one would exceed
this many characters.  A block with no blank line to divide it is split at
line boundaries instead, so a chapter-sized run of text does not become a
single passage."
  :type 'integer
  :group 'chai-index)

(defconst chai-index--schema-version 5
  "Schema version stored in the index.
A mismatch forces a full rebuild instead of silently querying stale rows.")

;;; Connection

(defvar chai-index--db nil
  "Open connection to `chai-index-file', or nil.")

(defvar chai-index--db-path nil
  "Path `chai-index--db' was opened against.")

(defun chai-index--check-sqlite ()
  "Signal a user error unless this Emacs can talk to SQLite."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error "This Emacs was built without SQLite support; Chai search is unavailable")))

(defun chai-index--stored-schema-version (db)
  "Return the schema version recorded in DB, or nil."
  (ignore-errors
    (when-let* ((row (car (sqlite-select db "SELECT value FROM meta WHERE key = 'schema'"))))
      (string-to-number (car row)))))

(defun chai-index--drop-schema (db)
  "Drop every Chai table in DB.

Dropping rather than emptying matters for `fts_chunks': its storage options
are fixed at creation, so a `CREATE ... IF NOT EXISTS' against an index built
by an older version would silently keep the old options while the code assumed
the new ones."
  (dolist (statement '("DROP TABLE IF EXISTS fts_chunks"
                       "DROP TABLE IF EXISTS chunks"
                       "DROP TABLE IF EXISTS sections"
                       "DROP TABLE IF EXISTS vec_sections"
                       "DROP TABLE IF EXISTS documents"
                       "DROP TABLE IF EXISTS meta"))
    (sqlite-execute db statement)))

(defun chai-index--create-schema (db)
  "Create the Chai index tables in DB if they are not already there."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS documents (
                        id INTEGER PRIMARY KEY,
                        path TEXT NOT NULL UNIQUE,
                        book_id TEXT,
                        title TEXT,
                        status TEXT,
                        keywords TEXT,
                        hash TEXT NOT NULL,
                        indexed_at INTEGER)")
  ;; Chunks reference the document by integer rather than repeating its path:
  ;; a real library has a few thousand paths of about ninety bytes shared by
  ;; hundreds of thousands of passages, and both the column and its index paid
  ;; for that repetition.
  ;; A section is a run of passages sharing one heading path.  It exists as a
  ;; row of its own because the semantic channel works at that granularity:
  ;; a passage is often too small to carry a topic, and embedding every one of
  ;; them costs several times as much for a coarser answer.
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS sections (
                        id INTEGER PRIMARY KEY,
                        doc INTEGER NOT NULL,
                        outline TEXT NOT NULL DEFAULT '',
                        beg_pos INTEGER NOT NULL,
                        end_pos INTEGER NOT NULL)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_sections_doc ON sections(doc)")
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS chunks (
                        id INTEGER PRIMARY KEY,
                        doc INTEGER NOT NULL,
                        section INTEGER,
                        headline TEXT,
                        outline TEXT NOT NULL DEFAULT '',
                        beg_pos INTEGER NOT NULL,
                        end_pos INTEGER NOT NULL,
                        preview TEXT NOT NULL,
                        hl_types TEXT NOT NULL DEFAULT '')")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_chunks_doc ON chunks(doc)")
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)")
  (condition-case err
      ;; `content=\'\'' stops FTS5 from storing a second copy of the tokenized
      ;; body, which `chunks.text' already covers; that alone is about a third
      ;; of the index.  `detail=none' would shrink it much further but is not
      ;; used: it discards term frequencies, and measured against real Chinese
      ;; text it drops the top results from passages mentioning a query dozens
      ;; of times to passages mentioning it once, which is no ranking at all.
      (sqlite-execute db "CREATE VIRTUAL TABLE IF NOT EXISTS fts_chunks
                          USING fts5(body, tokenize='unicode61',
                                     content='', contentless_delete=1)")
    (error
     (user-error "This Emacs's SQLite lacks FTS5, which Chai search requires: %s"
                 (error-message-string err))))
  (sqlite-execute db "INSERT OR REPLACE INTO meta (key, value) VALUES ('schema', ?)"
                  (list (number-to-string chai-index--schema-version))))

(defun chai-index--init-schema (db)
  "Prepare DB for use, rebuilding it from scratch when its schema is outdated."
  (sqlite-execute db "PRAGMA journal_mode = WAL")
  (sqlite-execute db "PRAGMA synchronous = NORMAL")
  (sqlite-execute db "PRAGMA cache_size = -32000")
  ;; A rebuild runs in a separate Emacs, so this connection shares the file with
  ;; a writer.  WAL already lets a reader and a writer coexist; the timeout
  ;; covers the moments they still collide.
  (sqlite-execute db "PRAGMA busy_timeout = 5000")
  (let ((version (chai-index--stored-schema-version db)))
    (when (and version (/= version chai-index--schema-version))
      (chai-index--drop-schema db)))
  (chai-index--create-schema db)
  db)

(defun chai-index--db ()
  "Return an open connection to `chai-index-file', creating it if needed."
  (chai-index--check-sqlite)
  (unless (and chai-index--db (equal chai-index--db-path chai-index-file))
    (when chai-index--db (ignore-errors (sqlite-close chai-index--db)))
    (make-directory (file-name-directory chai-index-file) t)
    (setq chai-index--db (sqlite-open chai-index-file)
          chai-index--db-path chai-index-file)
    (chai-index--init-schema chai-index--db))
  chai-index--db)

(defun chai-index--checkpoint (&optional truncate)
  "Fold the write-ahead log back into the database file.

SQLite checkpoints automatically, but only when no read transaction is in the
way; across a rebuild of thousands of books there always is one, so the log
grows without bound — measured at 9 GB beside a 2 GB database.  Checkpointing
explicitly keeps it bounded.  With TRUNCATE the log file is also shrunk to
nothing, which is worth the extra work when the rebuild has finished."
  (when chai-index--db
    (ignore-errors
      (sqlite-execute chai-index--db
                      (if truncate
                          "PRAGMA wal_checkpoint(TRUNCATE)"
                        "PRAGMA wal_checkpoint(PASSIVE)")))))

(defun chai-index-close ()
  "Close the index connection, if one is open."
  (interactive)
  (when chai-index--db
    (chai-index--checkpoint 'truncate)
    (ignore-errors (sqlite-close chai-index--db))
    (setq chai-index--db nil chai-index--db-path nil)))

;;; Tokenizer

;; `unicode61' treats an unbroken run of CJK as one token, so "分布式共识算法"
;; is a single term and a query for "算法" misses it.  Overlapping bigrams give
;; back word-level recall without a dictionary: the run is indexed as
;; "分布 布式 式共 共识 识算 算法".
;;
;; A run is bigrammed whole; only a run that is a single character on its own
;; contributes that character.  Emitting the trailing character of a longer run
;; as well would poison recall, because a tail like "法" is shared by unrelated
;; runs ("算法" and "做法") and the OR below would then match both.
;;
;; The write side joins tokens with a space; the query side joins them with OR
;; (see `chai-index-query-expression').  OR is not a preference: with FTS5's
;; implicit AND, a query for "分布式" becomes 分布 AND 布式, and any source that
;; spells the phrase differently is lost even though it shares a bigram.

(defconst chai-index--cjk-start #x2E80)
(defconst chai-index--cjk-end #xFFE5)

(defun chai-index--cjk-p (char)
  "Return non-nil when CHAR sits in the CJK range Chai bigrams."
  (and (>= char chai-index--cjk-start)
       (<= char chai-index--cjk-end)))

(defun chai-index--tokens (text)
  "Split TEXT into index tokens: CJK bigrams plus whole alphanumeric words."
  (let ((tokens nil)
        (i 0)
        (n (length text)))
    (while (< i n)
      (let ((char (aref text i)))
        (cond
         ((chai-index--cjk-p char)
          (let ((run i))
            (while (and (< i n) (chai-index--cjk-p (aref text i)))
              (setq i (1+ i)))
            (if (= (- i run) 1)
                (push (substring text run i) tokens)
              (dotimes (k (- i run 1))
                (push (substring text (+ run k) (+ run k 2)) tokens)))))
         ((string-match-p "[[:alnum:]_]" (string char))
          (let ((word i))
            (while (and (< i n)
                        (not (chai-index--cjk-p (aref text i)))
                        (string-match-p "[[:alnum:]_]" (string (aref text i))))
              (setq i (1+ i)))
            (push (substring text word i) tokens)))
         (t (setq i (1+ i))))))
    (nreverse tokens)))

(defun chai-index-index-text (text)
  "Return the tokenized form of TEXT stored in the FTS index."
  (string-join (chai-index--tokens text) " "))

(defun chai-index--quote-token (token)
  "Return TOKEN as an FTS5 string literal.
Quoting keeps characters like `-' and `:' from being read as query syntax."
  (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" token)))

(defun chai-index-query-expression (query)
  "Return the FTS5 MATCH expression for QUERY, or nil when it has no tokens."
  (when-let* ((tokens (chai-index--tokens query)))
    (string-join (mapcar #'chai-index--quote-token tokens) " OR ")))


;;; Token estimates

;; There is no tokenizer here, so the count is an estimate, deliberately on the
;; high side: a CJK character is usually its own token, while western text runs
;; roughly a token per three or four characters.  Overshooting wastes a little
;; of a model's window; undershooting silently loses whatever sat at the edge.

(defun chai-estimate-tokens (text)
  "Return a conservative estimate of how many tokens TEXT occupies."
  (let ((cjk 0) (other 0))
    (dolist (char (string-to-list (or text "")))
      (if (chai-index--cjk-p char)
          (setq cjk (1+ cjk))
        (setq other (1+ other))))
    (+ cjk (ceiling other 3))))

;;; Chunking

(defun chai-index--clean-text (text)
  "Return TEXT with Org scaffolding removed, ready for indexing and display.
Drawers and `#+' lines go away entirely; link markup collapses to the text a
reader actually sees, so a `chai:' highlight contributes its highlighted words
rather than its type name."
  (let ((clean text))
    (setq clean (replace-regexp-in-string
                 "^[ \t]*:\\(?:PROPERTIES\\|LOGBOOK\\):[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?"
                 "" clean))
    (setq clean (replace-regexp-in-string "^[ \t]*#\\+.*$" "" clean))
    (setq clean (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" clean))
    (setq clean (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" clean))
    (setq clean (replace-regexp-in-string "^[ \t]*\\*+[ \t]+" "" clean))
    (setq clean (replace-regexp-in-string "\n\\{3,\\}" "\n\n" clean))
    (string-trim clean)))

(defun chai-index--paragraph-regions (beg end)
  "Return (START . STOP) regions for the blank-line separated blocks in BEG..END.

The final block of a region has no blank line after it, so a failed search
means \"the rest of the text is one block\" and the scan is finished.  Treating
that case as ordinary progress instead would restart the scan one character
later and emit another block reaching to END, which is quadratic in the size
of a section — and sections without blank lines are the normal shape of a
book converted from PDF or EPUB."
  (let ((regions nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (skip-chars-forward " \t\n" end)
        (when (< (point) end)
          (let ((start (point)))
            (if (re-search-forward "\n[ \t]*\n" end t)
                (push (cons start (match-beginning 0)) regions)
              (push (cons start end) regions)
              (goto-char end))))))
    (nreverse regions)))

(defun chai-index--split-long-region (region)
  "Split REGION at line boundaries so no part exceeds `chai-index-chunk-max-chars'.

A section of a book converted from PDF or EPUB is often one unbroken run of
lines with no blank line anywhere in it.  Without this split such a section
would become a single passage the size of a chapter, which is useless as a
search result and flattens BM25 scoring across the whole book."
  (let ((beg (car region))
        (end (cdr region)))
    (if (<= (- end beg) chai-index-chunk-max-chars)
        (list region)
      (let ((parts nil)
            (start beg))
        (save-excursion
          (while (< start end)
            (goto-char (min end (+ start chai-index-chunk-max-chars)))
            (let* ((bol (line-beginning-position))
                   ;; Prefer a line boundary; fall back to a hard cut only when
                   ;; a single line is itself longer than the bound.
                   (stop (cond ((>= (point) end) end)
                               ((> bol start) bol)
                               (t (min end (+ start chai-index-chunk-max-chars))))))
              (push (cons start stop) parts)
              (setq start stop))))
        (nreverse parts)))))

(defconst chai-index--heading-re "^\\(\\*+\\)[ \t]+\\(.*\\)$"
  "Match an Org headline without needing Org mode to be enabled.
Group 1 is the leading stars, group 2 the rest of the line.")

(defconst chai-index--markup-re "\\[\\[chai:\\|#\\+BEGIN_CHAI"
  "Match any Chai annotation syntax.")

(defun chai-index--chai-markup-p ()
  "Return non-nil when the buffer contains any Chai annotation at all."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward chai-index--markup-re nil t)))

(defun chai-index--buffer-entries ()
  "Return the buffer's normalized Chai annotations, in source order.

Parsing the whole Org tree costs real time on a large book, and most books
carry no annotation at all, so a cheap regexp decides whether that parse is
worth doing.  A book with no Chai markup never pays for Org mode."
  (when (chai-index--chai-markup-p)
    (unless (derived-mode-p 'org-mode)
      (delay-mode-hooks (org-mode)))
    (chai--collect-entries)))

(defun chai-index--entry-type (entry)
  "Return the type ENTRY contributes to a chunk."
  (if (eq (plist-get entry :kind) 'comment)
      "comment"
    (plist-get entry :type)))

(defun chai-index--attach-types (chunks entries)
  "Record in each of CHUNKS the annotation types ENTRIES place inside it.
Both sequences are in source order, so one merge pass suffices; scanning all
entries for every chunk would be quadratic on a heavily annotated book."
  (let ((rest entries))
    (dolist (chunk chunks)
      (let ((beg (plist-get chunk :beg))
            (end (plist-get chunk :end))
            (types nil))
        (while (and rest (< (plist-get (car rest) :beg) beg))
          (setq rest (cdr rest)))
        (let ((cursor rest))
          (while (and cursor (< (plist-get (car cursor) :beg) end))
            (when-let* ((type (chai-index--entry-type (car cursor))))
              (push type types))
            (setq cursor (cdr cursor))))
        (plist-put chunk :types (delete-dups (nreverse types)))))
    chunks))

(defun chai-index--section-chunks (beg end headline outline)
  "Return chunk plists for the region BEG..END under HEADLINE.
Annotation types are attached afterwards by `chai-index--attach-types'."
  (let ((chunks nil)
        (pending nil)
        (pending-len 0))
    (cl-flet ((emit ()
                (when pending
                  (let* ((regions (nreverse pending))
                         (start (caar regions))
                         (stop (cdar (last regions)))
                         (text (chai-index--clean-text
                                (buffer-substring-no-properties start stop))))
                    (unless (string-empty-p text)
                      (push (list :beg start
                                  :end stop
                                  :headline headline
                                  :outline outline
                                  :text text
                                  :types nil)
                            chunks)))
                  (setq pending nil pending-len 0))))
      (dolist (block (chai-index--paragraph-regions beg end))
        (dolist (region (chai-index--split-long-region block))
          (let ((len (- (cdr region) (car region))))
            (when (and pending (> (+ pending-len len) chai-index-chunk-max-chars))
              (emit))
            (push region pending)
            (setq pending-len (+ pending-len len)))))
      (emit))
    (nreverse chunks)))

(defconst chai-index-blank-chars "[[:space:]　 ﻿]"
  "Character class of whitespace Chai treats as empty.
`string-trim' and `string-blank-p' only know ASCII whitespace, but headings in
books converted from Chinese ebooks are routinely padded with the ideographic
space U+3000, a non-breaking space, or a stray byte-order mark.")

(defun chai-index-trim (string)
  "Return STRING without leading or trailing whitespace, CJK included."
  (replace-regexp-in-string
   (concat "\\`" chai-index-blank-chars "+\\|" chai-index-blank-chars "+\\'")
   "" (or string "")))

(defun chai-index-blank-p (string)
  "Return non-nil when STRING holds nothing but whitespace, CJK included."
  (string-empty-p (chai-index-trim string)))

(defun chai-index--heading-title ()
  "Return the title of the Org headline matched by `chai-index--heading-re'.
Trailing tags and Org's `\\\\' line-break marker are dropped: neither is part of
the title a reader sees, and both show up in books converted from PDF or EPUB."
  (let ((line (chai-index-trim (match-string-no-properties 2))))
    (setq line (replace-regexp-in-string ":[[:alnum:]_@#%:]+:\\'" "" line))
    (setq line (replace-regexp-in-string "\\\\\\\\[ \t]*\\'" "" line))
    (chai-index-trim line)))

(defconst chai-index-outline-separator " › "
  "String joining the levels of a passage's headline path.")

(defun chai-index--outline-of (stack)
  "Return the display path for STACK, a list of (LEVEL . TITLE) innermost first.
Blank titles are left out rather than shown as empty path segments."
  (mapconcat #'identity
             (seq-remove #'chai-index-blank-p (mapcar #'cdr (reverse stack)))
             chai-index-outline-separator))

(defun chai-index--raw-chunks ()
  "Return untyped chunk plists for the current buffer, in source order.

Each chunk records both the headline directly above it and the full path of
headlines enclosing it, so a hit can be shown as \"Chapter › Section\" rather
than a section title with no indication of where in the book it sits."
  (let ((chunks nil))
    (save-excursion
      (goto-char (point-min))
      (let ((section-beg (point-min))
            (headline nil)
            (outline "")
            (stack nil))
        (while (re-search-forward chai-index--heading-re nil t)
          (let ((heading-beg (match-beginning 0))
                (level (length (match-string-no-properties 1)))
                (title (chai-index--heading-title)))
            (setq chunks (nconc chunks (chai-index--section-chunks
                                        section-beg heading-beg headline outline)))
            ;; Close every heading at this level or deeper before opening this one.
            (while (and stack (>= (caar stack) level))
              (pop stack))
            (push (cons level title) stack)
            (setq headline title
                  outline (chai-index--outline-of stack))
            (forward-line 1)
            (setq section-beg (point))))
        (setq chunks (nconc chunks (chai-index--section-chunks
                                    section-beg (point-max) headline outline)))))
    chunks))

(defun chai-index-buffer-chunks ()
  "Return chunk plists for the current buffer.
Each plist carries :beg, :end, :headline, :text and :types.  The positions
address the live buffer; the text is the cleaned copy used for search."
  (chai-index--attach-types (chai-index--raw-chunks) (chai-index--buffer-entries)))

;;; Indexing

(defun chai-index--file-hash (file)
  "Return a content hash for FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (secure-hash 'sha1 (current-buffer))))

(defun chai-index--delete-document (db path)
  "Remove PATH and every chunk belonging to it from DB."
  (when-let* ((doc (caar (sqlite-select db "SELECT id FROM documents WHERE path = ?" (list path)))))
    (dolist (row (sqlite-select db "SELECT id FROM chunks WHERE doc = ?" (list doc)))
      (sqlite-execute db "DELETE FROM fts_chunks WHERE rowid = ?" (list (car row))))
    (sqlite-execute db "DELETE FROM chunks WHERE doc = ?" (list doc))
    (dolist (row (sqlite-select db "SELECT id FROM sections WHERE doc = ?" (list doc)))
      (ignore-errors
        (sqlite-execute db "DELETE FROM vec_sections WHERE section_id = ?" (list (car row)))))
    (sqlite-execute db "DELETE FROM sections WHERE doc = ?" (list doc))
    (sqlite-execute db "DELETE FROM documents WHERE id = ?" (list doc))))

(defun chai-index--next-chunk-id (db)
  "Return the next free chunk id in DB."
  (1+ (or (caar (sqlite-select db "SELECT max(id) FROM chunks")) 0)))

(defun chai-index-preview (text)
  "Return at most `chai-index-preview-chars' characters of TEXT."
  (if (<= (length text) chai-index-preview-chars)
      text
    (concat (substring text 0 chai-index-preview-chars) "…")))

(defun chai-index--insert-chunk (db doc chunk id section)
  "Insert CHUNK of document DOC into DB under the explicit chunk ID.
The FTS row deliberately reuses that id as its rowid, so one JOIN later
carries every column a hit needs with no id mapping in between.  Assigning
ids here rather than reading `last_insert_rowid' back saves one round trip
per chunk, which matters at library scale."
  (sqlite-execute db "INSERT INTO chunks (id, doc, section, headline, outline, beg_pos, end_pos, preview, hl_types)
                      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                  (list id
                        doc
                        section
                        (or (plist-get chunk :headline) "")
                        (or (plist-get chunk :outline) "")
                        (plist-get chunk :beg)
                        (plist-get chunk :end)
                        (chai-index-preview (plist-get chunk :text))
                        (string-join (plist-get chunk :types) " ")))
  ;; Only the immediate headline joins the searchable body.  Indexing the whole
  ;; path would make every passage of a chapter match the chapter's title, which
  ;; buys recall at the cost of precision on exactly the broad queries where
  ;; precision matters most.
  (sqlite-execute db "INSERT INTO fts_chunks (rowid, body) VALUES (?, ?)"
                  (list id (chai-index-index-text
                            (concat (or (plist-get chunk :headline) "") "\n"
                                    (plist-get chunk :text))))))

(defun chai-index--index-book (db book hash)
  "Index BOOK into DB, recording HASH as its content fingerprint.
Return the number of chunks written."
  (let* ((path (chai-book-file-path book))
         (chunks (with-temp-buffer
                   (insert-file-contents path)
                   (chai-index-buffer-chunks)))
         (id (chai-index--next-chunk-id db)))
    (chai-index--delete-document db path)
    (sqlite-execute db "INSERT INTO documents (path, book_id, title, status, keywords, hash, indexed_at)
                        VALUES (?, ?, ?, ?, ?, ?, ?)"
                    (list path
                          (or (chai-book-id book) "")
                          (or (chai-book-title book) "")
                          (format "%s" (or (chai-book-status book) ""))
                          (string-join (or (chai-book-keywords book) nil) " ")
                          hash
                          (floor (float-time))))
    (let ((doc (caar (sqlite-select db "SELECT last_insert_rowid()")))
          (section nil)
          (section-outline :none))
      (dolist (chunk chunks)
        ;; A change of heading path opens a new section.
        (unless (equal section-outline (or (plist-get chunk :outline) ""))
          (setq section-outline (or (plist-get chunk :outline) ""))
          (sqlite-execute db "INSERT INTO sections (doc, outline, beg_pos, end_pos) VALUES (?, ?, ?, ?)"
                          (list doc section-outline
                                (plist-get chunk :beg) (plist-get chunk :end)))
          (setq section (caar (sqlite-select db "SELECT last_insert_rowid()"))))
        (sqlite-execute db "UPDATE sections SET end_pos = ? WHERE id = ? AND end_pos < ?"
                        (list (plist-get chunk :end) section (plist-get chunk :end)))
        (chai-index--insert-chunk db doc chunk id section)
        (setq id (1+ id))))
    (length chunks)))

(defun chai-index--book-hash-if-stale (db book)
  "Return BOOK's content hash when it needs indexing, or nil when it is current.
The hash is returned rather than recomputed by the caller: hashing a large
book twice per rebuild is pure waste at library scale."
  (let* ((path (chai-book-file-path book))
         (hash (chai-index--file-hash path))
         (row (car (sqlite-select db "SELECT hash FROM documents WHERE path = ?" (list path)))))
    (unless (equal (car row) hash) hash)))

;;; Rebuild

;; A first pass over a real library is minutes of work.  Running it inside the
;; editing session — even sliced across an idle timer — still spends that time
;; on the only thread the user has.  So the rebuild happens in a separate
;; batch Emacs, and this session only listens: the child prints one line per
;; book on stdout, a process filter turns those into a mode-line indicator, and
;; nothing else in the editing session is touched.
;;
;; Called from Lisp the same work runs synchronously in this process, which is
;; what tests and batch jobs want.

(defvar chai-index--job nil
  "State of a synchronous rebuild in progress, or nil.")

(defvar chai-index--process nil
  "Batch Emacs performing a rebuild, or nil.")

(defvar chai-index--progress nil
  "Mode-line string describing the rebuild in progress, or nil.")

(defvar chai-index--partial ""
  "Incomplete output line received from the rebuild process.")

(defvar chai-index--last-echo 0
  "When progress was last shown in the echo area.")

(defvar chai-index--noise nil
  "Recent output from the rebuild process that was not progress.
Kept so that a process dying early can say why.")

(defcustom chai-index-progress-in-echo-area t
  "Whether a rebuild reports progress in the echo area.

The mode line always carries the same information, but only for configurations
that render `global-mode-string' — many do not, and then the echo area is the
only place progress is visible at all."
  :type 'boolean
  :group 'chai-index)

(defcustom chai-index-echo-interval 1.0
  "Least number of seconds between two progress messages in the echo area."
  :type 'number
  :group 'chai-index)

(defun chai-index--job-init (force)
  "Return a fresh rebuild job over the Library, clearing the index when FORCE."
  (let ((db (chai-index--db))
        (books (chai-library-scan)))
    (when force
      (with-sqlite-transaction db
        (sqlite-execute db "DELETE FROM fts_chunks")
        (sqlite-execute db "DELETE FROM chunks")
        (sqlite-execute db "DELETE FROM documents")))
    (list :db db :pending books :total (length books) :done 0
          :indexed 0 :skipped 0 :chunks 0 :failed nil
          :paths (make-hash-table :test 'equal)
          :started (float-time))))

(defun chai-index--job-step (job)
  "Index the next book of JOB.  Return non-nil while books remain."
  (when-let* ((book (pop (plist-get job :pending))))
    (let ((path (chai-book-file-path book))
          (db (plist-get job :db)))
      (puthash path t (plist-get job :paths))
      (if-let* ((hash (chai-index--book-hash-if-stale db book)))
          ;; One unreadable book must not abort the whole rebuild.
          (condition-case err
              (let ((written (with-sqlite-transaction db
                               (chai-index--index-book db book hash))))
                (plist-put job :chunks (+ (plist-get job :chunks) written))
                (plist-put job :indexed (1+ (plist-get job :indexed))))
            (error (plist-put job :failed
                              (cons (cons path (error-message-string err))
                                    (plist-get job :failed)))))
        (plist-put job :skipped (1+ (plist-get job :skipped)))))
    (plist-put job :done (1+ (plist-get job :done))))
  (plist-get job :pending))

(defun chai-index--job-finish (job)
  "Drop books that vanished from the Library and report JOB's result."
  (let ((db (plist-get job :db))
        (paths (plist-get job :paths))
        (failed (plist-get job :failed)))
    (with-sqlite-transaction db
      (dolist (row (sqlite-select db "SELECT path FROM documents"))
        (unless (gethash (car row) paths)
          (chai-index--delete-document db (car row)))))
    (chai-index--checkpoint 'truncate)
    (list :indexed (plist-get job :indexed)
          :skipped (plist-get job :skipped)
          :chunks (plist-get job :chunks)
          :failed (mapcar #'car failed)
          :seconds (- (float-time) (plist-get job :started)))))

(defun chai-index--report (result)
  "Announce RESULT, the summary plist of a finished rebuild."
  (when-let* ((failed (plist-get result :failed)))
    (message "Chai index: %d file(s) skipped after errors: %s"
             (length failed)
             (mapconcat #'file-name-nondirectory failed ", ")))
  (message "Chai index: %d book(s) indexed, %d unchanged, %d passage(s), %.1fs"
           (plist-get result :indexed) (plist-get result :skipped)
           (plist-get result :chunks) (plist-get result :seconds))
  result)

;;; Rebuilding in a separate Emacs

(defconst chai-index--progress-prefix "CHAI-PROGRESS "
  "Marker beginning a progress line printed by the rebuild process.")

(defconst chai-index--result-prefix "CHAI-RESULT "
  "Marker beginning the summary line printed by the rebuild process.")

;;;###autoload
(defun chai-index-batch-run (settings)
  "Index the Library described by SETTINGS, reporting progress on stdout.

This is the entry point of the batch Emacs started by `chai-index-rebuild';
it is not meant to be called interactively.  SETTINGS is a plist of
:library, :index, :chunk-max, :preview and :force."
  (setq chai-library-directory (plist-get settings :library)
        chai-index-file (plist-get settings :index))
  (when-let* ((value (plist-get settings :chunk-max)))
    (setq chai-index-chunk-max-chars value))
  (when-let* ((value (plist-get settings :preview)))
    (setq chai-index-preview-chars value))
  (let ((job (chai-index--job-init (plist-get settings :force)))
        (more t))
    ;; Reported on stderr, not stdout: Emacs writes stderr straight through,
    ;; while stdout to a pipe is block-buffered and would deliver progress in
    ;; silent clumps of several kilobytes.
    (cl-flet ((emit (line) (princ line #'external-debugging-output))
              (report ()
                (princ (format "%s%d %d %d\n" chai-index--progress-prefix
                               (plist-get job :done) (plist-get job :total)
                               (plist-get job :chunks))
                       #'external-debugging-output)))
      (report)
      (while more
        (setq more (chai-index--job-step job))
        (report)
        (when (zerop (mod (plist-get job :done) 50))
          (chai-index--checkpoint)))
      (emit (format "%s%S\n" chai-index--result-prefix (chai-index--job-finish job))))))

(defun chai-index--emacs-program ()
  "Return the Emacs executable to run a rebuild in."
  (or (and invocation-directory
           (let ((path (expand-file-name invocation-name invocation-directory)))
             (and (file-executable-p path) path)))
      (executable-find "emacs")
      (user-error "Cannot find an Emacs executable to run the rebuild in")))

(defun chai-index--echo-progress (done total chunks)
  "Show DONE of TOTAL books and CHUNKS passages in the echo area.
Rate-limited, and silent while the minibuffer is in use so that a rebuild
never overwrites what the user is in the middle of typing."
  (when (and chai-index-progress-in-echo-area
             (not (active-minibuffer-window))
             (> (- (float-time) chai-index--last-echo) chai-index-echo-interval))
    (setq chai-index--last-echo (float-time))
    (let ((message-log-max nil))        ; progress does not belong in *Messages*
      (message "Indexing the Chai Library... %d/%d book(s), %d passage(s) — %s to stop"
               done total chunks
               (substitute-command-keys "\\[chai-index-stop]")))))

(defun chai-index--handle-line (line)
  "Act on one output LINE from the rebuild process."
  (cond
   ((string-prefix-p chai-index--progress-prefix line)
    (pcase-let ((`(,done ,total ,chunks)
                 (mapcar #'string-to-number
                         (split-string (substring line (length chai-index--progress-prefix))))))
      (setq chai-index--progress
            (format " Chai indexing %d/%d (%d)" done total chunks))
      (force-mode-line-update t)
      (chai-index--echo-progress done total chunks)))
   ((string-prefix-p chai-index--result-prefix line)
    (chai-index--report
     (car (read-from-string (substring line (length chai-index--result-prefix))))))
   ((not (string-blank-p line))
    ;; Anything else is a warning or a backtrace from the child.  Keep the tail
    ;; of it so an early death can be explained instead of just vanishing.
    (push line chai-index--noise)
    (setq chai-index--noise (seq-take chai-index--noise 10)))))

(defun chai-index--filter (process string)
  "Turn STRING from PROCESS into whole lines and act on each."
  (ignore process)
  (setq chai-index--partial (concat chai-index--partial string))
  (let ((lines (split-string chai-index--partial "\n")))
    ;; The last element is whatever arrived without a terminating newline.
    (setq chai-index--partial (car (last lines)))
    (dolist (line (butlast lines))
      (chai-index--handle-line (string-trim-right line)))))

(defun chai-index--sentinel (process event)
  "Clean up after the rebuild PROCESS ends with EVENT."
  (unless (process-live-p process)
    (let ((noise (nreverse chai-index--noise))
          (code (process-exit-status process)))
      (setq chai-index--process nil
            chai-index--progress nil
            chai-index--partial ""
            chai-index--noise nil)
      (setq global-mode-string (delq 'chai-index--progress global-mode-string))
      (force-mode-line-update t)
      (unless (or (string-prefix-p "finished" event) (zerop code))
        (message "Chai index: rebuild %s%s" (string-trim event)
                 (if noise (concat " — " (string-join (last noise 3) " / ")) ""))))))

;;;###autoload
(defun chai-index-stop ()
  "Stop a rebuild running in the background."
  (interactive)
  (if (not (process-live-p chai-index--process))
      (message "Chai index: no rebuild is running")
    (let ((done (and chai-index--progress (string-trim chai-index--progress))))
      (delete-process chai-index--process)
      ;; Every book already indexed was committed, so the next run resumes.
      (message "Chai index: stopped (%s) — run `chai-index-rebuild' to resume"
               (or done "no progress yet")))))

;;;###autoload
(defun chai-index-rebuild (&optional force)
  "Bring the Chai search index up to date with the Library.

Only books whose content changed are re-read.  With a prefix argument, or
when FORCE is non-nil, every book is re-indexed.

Run interactively the work happens in a separate Emacs process, so this
session is never blocked; progress appears in the mode line and
`chai-index-stop' ends it early.  Called from Lisp it runs synchronously and
returns a summary plist.  Either way each book is committed on its own, so
stopping leaves a consistent index and the next run resumes from there."
  (interactive "P")
  (if (not (called-interactively-p 'any))
      (let ((job (chai-index--job-init force))
            (reporter nil))
        (setq reporter (make-progress-reporter "Indexing the Chai Library" 0 (plist-get job :total)))
        (unwind-protect
            (while (chai-index--job-step job)
              (progress-reporter-update reporter (plist-get job :done))
              (when (zerop (mod (plist-get job :done) 50))
                (chai-index--checkpoint)))
          (progress-reporter-done reporter))
        (chai-index--report (chai-index--job-finish job)))
    (when (process-live-p chai-index--process)
      (user-error "A Chai index rebuild is already running; stop it with `chai-index-stop'"))
    ;; Hand the database over: two writers on one file would only contend.
    (chai-index-close)
    (let* ((directory (file-name-directory (locate-library "chai-index")))
           (settings (list :library chai-library-directory
                           :index chai-index-file
                           :chunk-max chai-index-chunk-max-chars
                           :preview chai-index-preview-chars
                           :force (and force t))))
      (setq chai-index--partial ""
            chai-index--noise nil
            chai-index--last-echo 0
            chai-index--progress " Chai indexing…")
      (unless (memq 'chai-index--progress global-mode-string)
        (setq global-mode-string (append global-mode-string '(chai-index--progress))))
      (setq chai-index--process
            (make-process
             :name "chai-index"
             :buffer nil
             :noquery t
             :connection-type 'pipe
             :command (list (chai-index--emacs-program)
                            "-Q" "--batch"
                            "-L" directory
                            "-l" "chai-index"
                            "--eval" (format "(chai-index-batch-run '%S)" settings))
             :filter #'chai-index--filter
             :sentinel #'chai-index--sentinel))
      (message "Chai index: rebuilding in a separate Emacs; %s to stop"
               (substitute-command-keys "\\[chai-index-stop]"))
      nil)))

(defun chai-index-book-at (path)
  "Return the Library book PATH holds, or nil when it is not one.

The filename is parsed directly rather than scanning the whole Library: a
scan re-reads a few thousand entries, which is far too much work to do every
time one file is saved."
  (when (and path
             (string-suffix-p ".org" path)
             (file-readable-p path)
             (file-equal-p (file-name-directory path) chai-library-directory))
    (chai-library--parse-filename (file-name-nondirectory path)
                                  chai-library-directory)))

;;;###autoload
(defun chai-index-update-file (&optional file)
  "Re-index FILE (default: the file the current buffer visits)."
  (interactive)
  (let* ((path (or file (buffer-file-name)))
         (book (and path (chai-index-book-at path))))
    (cond
     ((null path) (user-error "This buffer is not visiting a file"))
     ((null book) (user-error "Not a book in the Chai Library: %s" path))
     (t (let ((db (chai-index--db)))
          (with-sqlite-transaction db
            (chai-index--index-book db book (chai-index--file-hash path)))
          (message "Chai index: updated %s" (file-name-nondirectory path)))))))


;;; Keeping the index up to date

;; Saving a book should not stall on re-indexing it, and re-indexing on every
;; keystroke of a long editing session is wasted work.  So a save only records
;; that the file changed, and the work happens once Emacs falls idle.

(defcustom chai-index-auto-delay 5
  "Seconds of idleness before a saved book is re-indexed."
  :type 'number
  :group 'chai-index)

(defvar chai-index--dirty nil
  "Books saved since the index last caught up.")

(defvar chai-index--auto-timer nil
  "Idle timer that re-indexes saved books, or nil.")

(defun chai-index--note-save ()
  "Record that the file this buffer visits needs re-indexing."
  (when-let* ((path (buffer-file-name)))
    (when (chai-index-book-at path)
      (cl-pushnew path chai-index--dirty :test #'equal))))

(defun chai-index--catch-up ()
  "Re-index the books saved since the last time this ran."
  (when (and chai-index--dirty (not (process-live-p chai-index--process)))
    (let ((pending chai-index--dirty)
          (indexed 0))
      (setq chai-index--dirty nil)
      (dolist (path pending)
        ;; A book deleted or renamed since the save is simply no longer ours.
        (when-let* ((book (chai-index-book-at path)))
          ;; One unreadable book must not stop the others.
          (condition-case err
              (let ((db (chai-index--db)))
                (with-sqlite-transaction db
                  (chai-index--index-book db book (chai-index--file-hash path)))
                (setq indexed (1+ indexed)))
            (error (message "Chai index: could not update %s: %s"
                            (file-name-nondirectory path)
                            (error-message-string err))))))
      (when (> indexed 0)
        (chai-index--checkpoint)
        (let ((message-log-max nil))
          (message "Chai index: updated %d book(s)" indexed))))))

;;;###autoload
(define-minor-mode chai-index-auto-mode
  "Keep the search index in step with the Library as you edit it.

A book you save is re-indexed once Emacs has been idle for
`chai-index-auto-delay' seconds, so saving stays instant and a long editing
session costs one re-index rather than one per save."
  :global t
  :group 'chai-index
  (if chai-index-auto-mode
      (progn
        (add-hook 'after-save-hook #'chai-index--note-save)
        (setq chai-index--auto-timer
              (run-with-idle-timer chai-index-auto-delay t #'chai-index--catch-up)))
    (remove-hook 'after-save-hook #'chai-index--note-save)
    (when chai-index--auto-timer
      (cancel-timer chai-index--auto-timer)
      (setq chai-index--auto-timer nil))
    (setq chai-index--dirty nil)))

;;;###autoload
(defun chai-index-status ()
  "Report what the Chai search index currently holds."
  (interactive)
  (let* ((db (chai-index--db))
         (docs (caar (sqlite-select db "SELECT count(*) FROM documents")))
         (chunks (caar (sqlite-select db "SELECT count(*) FROM chunks")))
         (latest (caar (sqlite-select db "SELECT max(indexed_at) FROM documents"))))
    (message "Chai index: %d book(s), %d chunk(s), last updated %s (%s)"
             (or docs 0) (or chunks 0)
             (if latest
                 (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time latest))
               "never")
             chai-index-file)
    (list :documents (or docs 0) :chunks (or chunks 0) :updated latest)))

;;;###autoload
(defun chai-index-reset ()
  "Delete the Chai search index entirely.  The next rebuild recreates it."
  (interactive)
  (when (yes-or-no-p (format "Delete the Chai search index at %s? " chai-index-file))
    (chai-index-close)
    (dolist (suffix '("" "-wal" "-shm"))
      (let ((file (concat chai-index-file suffix)))
        (when (file-exists-p file) (delete-file file))))
    (message "Chai index deleted")))

(provide 'chai-index)

;;; chai-index.el ends here
