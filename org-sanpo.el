;; org-sanpo.el --  -*- lexical-binding: t; -*-
;;

(require 'org-capture)
(require 'org-protocol)
(require 'org-id)
;; apply: Symbol’s function definition is void: magit-process-file
;; 上記エラーを回避するために magit-process を require
(require 'magit-process)
(require 'magit-git)
(require 's)
(require 'f)
(require 'map)
(require 'emacsql)
(require 'emacsql-sqlite)

;; * 基本設定

;; Must be root of git repository too.
(defvar org-sanpo-directory "~/org/")

(defun org-sanpo-directory ()
  (f-full org-sanpo-directory))

(defconst org-sanpo--cache-db-filename ".org-sanpo-cache.sqlite")

(defun org-sanpo-cache-db-file ()
  (f-full (f-join org-sanpo-directory org-sanpo--cache-db-filename)))

(defvar org-sanpo-debug nil)

(defun org-sanpo--assert-sanpo-directory ()
  "Must be a root directory, and the repository needs at least one commit."
  (let* ((toplevel (magit-rev-parse "--show-toplevel"))
         (commit (magit-rev-parse "HEAD")))
    (unless toplevel (error "Not in git repository."))
    (unless (string= (f-slash (f-canonical default-directory)) (f-slash toplevel))
      (error (concat "default-directory: " default-directory " is not a root of git repository(" toplevel ")")))
    (unless commit (error "Not in git repository."))
    t))

(defmacro with-sanpo-directory (&rest form)
  `(let ((default-directory (org-sanpo-directory)))
     (org-sanpo--assert-sanpo-directory)
     ,@form))

;; * org-sanpo-mode

(defvar org-sanpo-mode-map (make-sparse-keymap))

(define-minor-mode org-sanpo-mode
  "Minor mode for Org-sanpo."
  :global t
  :keymap org-sanpo-mode-map
  (with-sanpo-directory
   (cond
    (org-sanpo-mode
     (org-sanpo--init-cache)
     ;; WARN: Overwrites `org-id' settings for type "id".
     ;; NOTE: Maybe we can specify :face property to do interest things.
     ;; Like if the pointing headline is DONE state, use a sticke-through face.
     (org-link-set-parameters "id" :follow #'org-sanpo-id-open))
    (t
     (org-sanpo--close-cache)
     ;; id について既に独自設定されている可能性がある。
     ;; そのため上書き前の設定を保持しておいて元に戻すほうがいいかな。
     (org-link-set-parameters "id" :follow #'org-id-open)))))

;; * find-file/insert-link コマンド
;;
;; 基本的に org-roam が提供する関数と同等のものを提供する。

(defun org-sanpo-find-file ()
  "Find and open an Org-sanpo file."
  (interactive)
  (with-sanpo-directory
   (let* ((completions (org-sanpo--get-headline-completions))
          ;; (title (completing-read "Headlines: " completions))
          (str0 (completing-read "Headline: " completions))
          (str (or (-find (lambda (x) (equal x str0)) completions) str0))
          (headline (get-text-property 0 'org-sanpo-headline str)))
     (if headline
         (pcase-let ((`(,file ,id _) headline))
           (pop-to-buffer-same-window
            (org-sanpo--get-or-create-headline-buffer file id)))
       (apply 'org-sanpo--new-headline-capture
              (funcall org-sanpo-new-headline-props-function `(find-file ,str)))))))

;; 入れ子の capture も可能。
;; ただこれ abort した場合でも link は残ってしまう。
;; 現状自分で消す必要あり。
(defun org-sanpo-insert-link ()
  (interactive)
  (with-sanpo-directory
   (let* ((region (and (region-active-p) (cons (region-beginning) (region-end))))
          (region-text (when region (buffer-substring-no-properties (car region) (cdr region))))
          (completions (org-sanpo--get-headline-completions))
          (select0 (completing-read "Headline: " completions nil nil region-text))
          (select (-find (lambda (x) (equal x select0)) completions))
          (headline (get-text-property 0 'org-sanpo-headline select))
          (new-props (unless headline (funcall org-sanpo-new-headline-props-function `(insert-link ,select))))
          (link-desc (or region-text (and headline (nth 2 headline)) select))
          (link-id (or (and headline (nth 1 headline)) (nth 1 new-props))))
     ;; Remove previously selected text.
     (when region
       (delete-region (car region) (cdr region)))
     (insert (org-sanpo--format-link link-id link-desc))
     (when new-props
       (apply 'org-sanpo--new-headline-capture new-props)))))

(defun org-sanpo--format-link (id desc)
  (format "[[id:%s][%s]]"
          id
          (s-replace-all '(("[" . "［") ("]" . "］")) desc)))

;; * daily コマンド

(defun org-sanpo--daily-id (time)
  "Fixed format ID for daily headlines."
  (format-time-string "SANPO-DAILY-%Y%m%d" time))

(defun org-sanpo-today ()
  "Open today's daily note."
  (interactive)
  (org-sanpo-daily (current-time)))

(defun org-sanpo-yesterday ()
  "Open yesterday's daily note."
  (interactive)
  (org-sanpo-daily (time-add -86400 (current-time))))

;; TODO: Make auto-save/auto-staging behaviour optional
(defun org-sanpo-daily (time)
  "Open daily headline that includes time.
It tries extra hard to not create duplicated headlines.
Also it auto-save/auto-staging to prevent future duplication."
  (interactive)
  (with-sanpo-directory
   (let* ((id (org-sanpo--daily-id time))
          (file (format-time-string "sanpo/%Y%m%d.org" time))
          (title (format-time-string "%Y/%m/%d(%a)" time))
          (file-buffer (find-file-noselect file))
          (headline (org-sanpo--get-headline id)))
     (pop-to-buffer-same-window
      (cond (headline
             (let ((file (cadr headline)))
               (org-sanpo--get-or-create-headline-buffer file id)))
            ((and file-buffer (with-current-buffer file-buffer (org-find-entry-with-id id)))
             ;; While the file doesn't exist on disk,
             ;; there might be a file visiting buffer which includes target id.
             ;; This could happen when you revert new created file with git.
             ;; Save + Staging first
             (with-current-buffer file-buffer (save-buffer))
             (unless (magit-git-success "add" file) (error "Failed staging file"))
             (org-sanpo--get-or-create-headline-buffer file id))
            (t
             (let ((org-sanpo--setup-headline-buffer-function nil))
               (org-sanpo--new-headline-capture file id title '("daily")))
             (org-capture-finalize)
             ;; Add to staging immediatly to prevent duplication
             (unless (magit-git-success "add" file) (error "Failed staging file"))
             (org-sanpo--get-or-create-headline-buffer file id)))))))

;; * 既存 headline に narrow された buffer の取得

;; subtree に narrow されている前提で。
(defvar org-sanpo--setup-headline-buffer-function nil)

(defun org-sanpo--get-or-create-headline-buffer (file id)
  "対象ファイルの indrect buffer を作成し返す。
既に存在している場合はその buffer を単に返す。
indirect buffer の名前は (f-base <file>)#<id> とする。id が一意的なため
org-mode は有効、対象の headline に narrow された状態にする。"
  (let* ((buffer-name (concat (f-base file) "#" id))
         (buffer (get-buffer buffer-name)))
    (if buffer
        buffer
      (let* ((file-buffer (find-file-noselect file))
             (buffer (make-indirect-buffer file-buffer buffer-name 'clone)))
        (with-current-buffer buffer
          ;; NOTE: キャッシュDBには headline の開始位置(begin)を持っているが、
          ;; 対象ファイルを編集中(未保存)の場合は開始位置がキャッシュとズレている可能性があるため
          ;; file 内 id 検索を用いる。
          ;; TODO: Its not guaranteed that file has id headline. Need to handle such case.
          (goto-char (org-find-entry-with-id id))
          (org-narrow-to-subtree)
          ;; TODO: point が headline の開始位置より内容の先頭(affliated-content?)にあるほうが便利かな
          (org-show-subtree)
          (when (functionp org-sanpo--setup-headline-buffer-function)
            (funcall org-sanpo--setup-headline-buffer-function)))
        buffer))))

;; * 新規 headline を作成 (org-capture を利用)
;;
;; 名前が悪いな...
;; 現状 org-capture 使っているが、要らないかも...

;; data NewContext
;;   = FindFile String
;;   | InsertLink String
;;   | Protocol String(title) String(url) String(selection(opt))
;;
(defvar org-sanpo-new-headline-props-function
  'org-sanpo--default-new-headline-props)

(defun org-sanpo--default-new-headline-props (arg)
  (let ((file (format-time-string "sanpo/%Y%m%d.org"))
        (id (org-id-new)))
    (pcase arg
      (`(find-file ,str)
       (list file id str))
      (`(insert-link ,str)
       (list file id str))
      (`(protocol ,title ,url ,selection)
       (let* ((meta (format "#+NAME: meta\n * source :: %s\n" url))
              (selection (when (and selection (not (s-blank? selection)))
                           (concat "#+BEGIN_QUOTE\n" (s-trim selection) "\n#+END_QUOTE\n")))
              (initial-content (concat meta "\n" selection))
              (properties `(("SOURCE" . ,url)))
              (tags '("web")))
         (when (s-contains? ".youtube." url) (push "video" tags))
         (when (s-contains? "twitter.com" url) (push "tweet" tags))
         (list file id title tags properties initial-content)))
      (_
       (error "Unexpected new headline context %s" arg)))))

(defun org-sanpo--new-headline-capture (file id title &optional tags properties initial-content)
  "Create heading with given `title' and `id'.
Uses org-capture feature.
`id' must be format that recoignesed by `org-id'.
Though the limitation looks like don't include white space."
  (let* ((file (if (f-relative? file) (f-join (org-sanpo-directory) file) file))
         (target `(file ,file))
         (template (concat "* " title "\n\n" initial-content "%?"))
         (props '(:prepend t :empty-lines-after 1))
         (org-capture-entry `("_" "_" entry ,target ,template ,@props)))
    (org-capture)
    (org-entry-put nil "ID" id)
    (org-entry-put nil "CREATED_AT" (format-time-string "%s"))
    (pcase-dolist (`(,name . ,val) properties)
      (org-entry-put nil name val))
    (when tags
      ;; `org-set-tags' assumes point is on headline,
      ;; so we need to move to the headline first.
      (save-excursion
        (goto-char (point-min))
        (org-set-tags tags)))
    ;; 何故上のほうが見切れているので recenter する
    (recenter -1)
    (when (functionp org-sanpo--setup-headline-buffer-function)
      (funcall org-sanpo--setup-headline-buffer-function))))

;; * org-link-set-parameters

;; TODO: Failback to `org-id-open' if current file is not under `org-sanpo-directory'??
(defun org-sanpo-id-open (id)
  (with-sanpo-directory
   (let ((headline (org-sanpo--get-headline id)))
     (if headline
         (let ((id (car headline))
               (file (cadr headline)))
           (pop-to-buffer (org-sanpo--get-or-create-headline-buffer file id)))
       (error "No headline with id = %s" id)))))

;; * org-protocol support

;; Must return non-nil if valid, otherwise return ni.
;; Excepts new-style link (e.g. key1=val1&key2=val2)
(defun org-sanpo-org-protocol-handler (plist)
  (with-sanpo-directory
   (when-let ((title (plist-get plist :title))
              (url (plist-get plist :url)))
     (let* ((selection (plist-get plist :selection))
            (props (funcall org-sanpo-new-headline-props-function
                            (list 'protocol title url selection))))
       (apply #'org-sanpo--new-headline-capture props)
       t))))

(add-to-list 'org-protocol-protocol-alist
             '("Org Sanpo"
               :protocol "sanpo"
               :function org-sanpo-org-protocol-handler))

;; * 候補
;;
;; 候補は表示文字列のリストで返す。
;; メタ情報は `org-sanpo-headline' テキスト属性として文字列全体に追加する。
;; 結果の文字列からはテキスト属性が削られているため `completing-read' を使った補完を難しくしている。
;; `completing-read' を使うのであれば候補に一意性を持たせる必要がある。

(defun org-sanpo--get-headline-completions ()
  (let* ((conn (org-sanpo--get-cache))
         (rows (emacsql conn "
SELECT h.id, h.file, h.title, h.todo_keyword, '(' || GROUP_CONCAT(t.tag, ' ') || ')'
FROM headlines h LEFT JOIN tags t ON h.id = t.id
GROUP BY h.id, h.file, h.title, h.todo_keyword")))
    (-map (pcase-lambda (`(,id ,file ,title ,todo-keyword ,tags))
            (let ((right (propertize file 'face 'font-lock-comment-face))
                  (direct-prefix (when todo-keyword (concat (propertize todo-keyword 'face (org-get-todo-face todo-keyword)) " ")))
                  (direct-suffix (when tags (propertize (concat " :" (s-join ":" tags) ":") 'face 'org-tag))))
              (propertize (concat direct-prefix title direct-suffix)
                          'org-sanpo-headline (list file id title)
                          'selectrum-candidate-display-right-margin right )))
          rows)))

;; * Retrieve from DB

(defun org-sanpo--get-headline (id)
  (let* ((conn (org-sanpo--get-cache))
         (rows (emacsql conn (format "
SELECT h.id, h.file, h.title, '(' || GROUP_CONCAT(t.tag, ' ') || ')'
FROM headlines h LEFT JOIN tags t ON h.id = t.id
WHERE h.id = '%S'
GROUP BY h.id, h.file, h.title" id))))
    (pcase rows
      (`(,v) v)
      (_ nil))))

;; * Git関連

(defun org-sanpo--org-files-for-commit (commit &optional paths)
  "Get the list of org files at the time of the commit。
各ファイルは (path . object-hash) の形を取る。
`default-directory' が git repo のルートに設定されていることを前提とする。"
  (let* ((raw (apply #'magit-git-items "ls-tree" "-r" "-z" commit paths))
         (parsed (-map 'org-sanpo--parse-ls-tree-output raw))
         (filtered (-filter (lambda (x) (and (string= (aref x 1) "blob") (f-ext? (aref x 3) "org")))
                            parsed)))
    (-map (lambda (x) (cons (aref x 3) (aref x 2))) filtered)))

(defun org-sanpo--parse-ls-tree-output (str)
  "<mode> SP <type> SP <object> TAB <file>.
returns (vector <mode> <type> <object> <file>)."
  (pcase (s-split-up-to "\t" str 1)
    (`(,before ,file)
     (pcase (s-split " " before)
       (`(,mode ,type ,object) (vector mode type object file))
       (_ (error ("Unexpected format")))))
    (_ (error "Unexpected format"))))

;; -z option を与えた場合 ("A" "a.org" "M" "b.org") のような形式になる
;; TODO: --name-status 渡さなければ object-hash が得られるようだ
(defun org-sanpo--git-tree-diff (from-commit to-commit)
  "二つのコミット間のファイルの変化を返す。"
  (--filter (f-ext? (plist-get it :file) "org")
            (-map 'org-sanpo--parse-diff-lines
                  (-partition 2
                              (magit-git-items "diff-tree" "-r" "-z" "--name-status" from-commit to-commit)))))

(defun org-sanpo--parse-diff-lines (x)
  (list :type (pcase (car x)
                ("M" 'modify)
                ("A" 'add)
                ("D" 'delete)
                (_ (error "Unexpectd type: %s" (aref str 0))))
        :file (cadr x)))

;; TODO: diff の検出は .org ファイルに限定してもいいかも..
(defun org-sanpo--git-diff-index-have-cached-diff (commit)
  "If there is diff return t, otherwise return nil.
Diff mean difference in index from commit."
  (magit-git-failure "diff-index" "--cached" "--quiet" commit))

;; NOTE: git gc されちゃうと消えてしまう。root を張る方法ってあるのかな？
(defun org-sanpo--git-create-volatile-commit-from-index (parent-commit message)
  "Create a voliatile commit object from index.
Volatile means this commit won't be in commit graph.
Which means `git gc' will delete this commit object."
  (let* ((tree (magit-git-string "write-tree"))
         (commit (magit-git-string "commit-tree" "-p" parent-commit "-m" message tree)))
    commit))

;; * Orgファイルから情報抽出

(defun org-sanpo--extract-from-file (file &optional object-hash)
  (with-temp-buffer
    (if object-hash
        (insert (magit-git-output "cat-file" "blob" object-hash))
      (insert-file-contents file))
    (let ((org-startup-with-latex-preview nil)
          (org-startup-with-inline-images nil))
      ;; To extract todo-keyword we need to enable `org-mode'
      ;; ref: https://emacs.stackexchange.com/questions/35042/parsing-an-orgmode-file-with-org-element-parse-buffer
      ;; NOTE: Feels there is more efficient way.
      (org-mode))
    (org-sanpo--extract file)))

;; Current buffer major must be `org-mode'.
(defun org-sanpo--extract (file)
  (let* ((structure (org-element-parse-buffer))
         (headlines-and-tags (org-sanpo--extract-headlines-and-tags structure file))
         (links (org-sanpo--extract-links structure file)))
    `(:links ,links ,@headlines-and-tags)))

;; :begin は最初の * の前の point
;; :end は headline のコンテンツの終了位置(入れ子 headling はコンテンツの一部)
;; 同じレベル(もしくは小さい)の headling が続いている場合、次のheadling の begin と一致する
;; `org-structure' は `(org-element-parse-buffer)' の返り値
(defun org-sanpo--extract-headlines-and-tags (org-structure file)
  (let ((x (org-element-map org-structure 'headline
             (lambda (hl)
               (when-let ((id (org-element-property :ID hl)))
                 (cons (vector id
                               file
                               (org-element-property :raw-value hl)
                               (org-element-property :level hl)
                               (org-element-property :begin hl)
                               (org-element-property :todo-keyword hl))
                       (-map (lambda (tag) (vector id tag)) (org-element-property :tags hl))))))))
    (list :headlines (-map #'car x)
          :tags (-flatten (-map #'cdr x)))))


;; NOTE: org-id を require しないと `id:aaaa-ef' のフォーマットは fuzzy と判別されてしまう...
;; 恐らく org-id が hyperlinks に登録しているっぽいな
(defun org-sanpo--extract-links (org-structure file)
  (org-element-map org-structure 'link
    (lambda (link)
      (when (string= "id" (org-element-property :type link))
        (let* ((element (org-sanpo--nearest-element link))
               (context-begin (or (org-element-property :content-begin element) (org-element-property :begin element)))
               (context-end (or (org-element-property :content-end element) (org-element-property :end element)))
               (context (or (org-element-property :raw-value element) (buffer-substring context-begin context-end)))
               (context (string-trim context))
               (ancestors (--unfold (let ((x (org-element-property :parent it))) (and x (cons x x))) element))
               (headlines (--filter (eq (org-element-type it) 'headline) ancestors)))
          (vector (-first 'identity (--map (org-element-property :ID it) headlines))
                  (and headlines (org-element-property :raw-value (car headlines)))
                  file
                  ;; raw-link は link の部分(例えば URL や `id:aaaa' の部分だけ)
                  (s-chop-prefix "id:" (org-element-property :raw-link link))
                  (buffer-substring (org-element-property :begin link) (org-element-property :end link))
                  (org-element-property :begin link)
                  context
                  context-begin))))))

(defun org-sanpo--nearest-element (x)
  (while (and x (not (member (org-element-type x) org-element-all-elements)))
    (setq x (org-element-property :parent x)))
  x)

;; * DBキャッシュ

;; NOTE: 際ロード時に備えて defvar ではなくて意図的に setq にしている。
;; 際ロードすると `org-sanpo--cache-schema-version' が更新されている可能性があり、
;; その際は `org-sanpo--init-cache' を再度走らせる必要があるため。
;;
;; plist :conn, :db-file
(setq org-sanpo--cache nil)

(defun org-sanpo--current-commit (conn)
  "cacheが構築されているコミット。
コミットツリーに存在いないコミットの可能性がある。"
  (pcase (emacsql conn [:select [commit index-commit] :from commit])
    (`((,commit ,index-commit)) (or index-commit commit))
    (_ (error "Unexpected schema"))))

(defun org-sanpo--current-head-commit (conn)
  (caar (emacsql conn [:select commit :from commit])))

(defun org-sanpo--current-index-commit (conn)
  (caar (emacsql conn [:select index-commit :from commit])))

(defun org-sanpo--init-cache ()
  "キャッシュDBに接続して `org-sanpo--cache' に設定する。
初回(もしくは schema versionが変わった場合)は HEADコミットからキャッシュを
ビルドするため時間がかかる。既にキャッシュDBが存在する場合更新は行なわない。"
  (let* ((db-file (org-sanpo-cache-db-file))
         (conn (org-sanpo--db-connect-or-create db-file))
         (user-version (caar (emacsql conn "PRAGMA user_version"))))
    (setq org-sanpo--cache
          (if (eq user-version org-sanpo--cache-schema-version)
              (list :conn conn
                    :db-file db-file)
            (let ((commit (magit-rev-parse "HEAD")))
              (org-sanpo--create-cache-from-scratch conn commit)
              (list :conn conn
                    :db-file db-file))))))

(defun org-sanpo--close-cache ()
  (when org-sanpo--cache
    (let ((conn (plist-get org-sanpo--cache :conn)))
      (emacsql-close conn)
      (setq org-sanpo--cache nil))))

(defun org-sanpo--get-cache ()
  "使える状態のキャッシュを返す。
キャッシュの形式は :conn,:db-file から成る plist。
必要があれば初回ビルド、最新のコミットに合わせた更新がされる。"
  (unless org-sanpo--cache (org-sanpo--init-cache))
  (let* ((head-commit (magit-rev-parse "HEAD"))
         (conn (plist-get org-sanpo--cache :conn)))
    ;; Update head-commit if needed
    (unless (string= head-commit (org-sanpo--current-head-commit conn))
      (when org-sanpo-debug (message "[org-sanpo] Update to head commit: %s" head-commit))
      (org-sanpo--update-cache-to-commit conn head-commit))
    ;; Update index-commit if needed
    (when (org-sanpo--git-diff-index-have-cached-diff (org-sanpo--current-commit conn))
      (let ((index-commit (org-sanpo--git-create-volatile-commit-from-index
                           (org-sanpo--current-commit conn)
                           "volatile commit for org-sanpo")))
        (when org-sanpo-debug (message "[org-sanpo] Update to index commit: %s" index-commit))
        (org-sanpo--update-cache-to-commit conn index-commit t)))
    conn))

(defun org-sanpo--update-cache-to-commit (conn commit &optional is-index-commit)
  "現状効率的な実装になっていない"
  (emacsql-with-transaction conn
    (let* ((current-commit (org-sanpo--current-commit conn))
           (diffs (org-sanpo--git-tree-diff current-commit commit))
           (delete-files (--map (plist-get it :file) diffs))
           (insert-files (--map (plist-get it :file) (--filter (member (plist-get it :type) '(modify add)) diffs)))
           (org-files (and insert-files (org-sanpo--org-files-for-commit commit insert-files))))
      ;; `org-sanpo--org-files-for-commit' は内部で ls-tree 使っていて、paths 引
      ;; 数はあくまでもパターンマッチングとして使われる。ファイルしかないはずな
      ;; ので、変なファイル名(他のファイルの prefix なにっている)がない限り問題
      ;; はないはず。
      (unless (equal (sort insert-files 'string<) (sort (-map 'car org-files) 'string<))
        (error "Uexpected org-files"))
      ;; modify/add/delete のファイルを一旦削除。
      ;; 外部キー制約によって関連する headlines/links/tags も削除されるはず。
      (emacsql conn (vector :delete :from 'files :where `(in file ,(apply 'vector delete-files))))
      ;; modify/add のファイルのみ
      (org-sanpo--insert-org-files conn org-files)
      (emacsql conn
               (if is-index-commit
                   (vector :update 'commit :set `(= index-commit ,commit))
                 (vector :update 'commit :set (vector `(= commit ,commit) `(= index-commit ,nil))))))))

(defun org-sanpo--create-cache-from-scratch (conn commit)
  "初回の利用時及びscheme の更新があった際に実行される。
テーブルはもし存在するなら一旦全削除され、最新の schema で作成され、
schema-version が設定される。"
  (emacsql-with-transaction conn
    (pcase-dolist (`(,table . _) org-sanpo--cache-schema)
      (emacsql conn [:drop-table :if-exists $i1] table))
    (pcase-dolist (`(,table . ,schema) org-sanpo--cache-schema)
      (emacsql conn [:create-table $i1 $S2] table schema))
    (org-sanpo--insert-org-files conn (org-sanpo--org-files-for-commit commit))
    (emacsql conn [:delete :from commit])
    (emacsql conn (vector :insert :into 'commit :values (vector commit nil)))
    (emacsql conn (format "PRAGMA user_version = %s" org-sanpo--cache-schema-version))))

(defun org-sanpo--insert-org-files (conn org-files)
  (when org-files
    (emacsql conn (vector :insert :into 'files :values (--map (vector (car it) (cdr it)) org-files)))
    (pcase-dolist (`(,file . ,object-hash) org-files)
      (let* ((x (org-sanpo--extract-from-file file object-hash))
             (headlines (plist-get x :headlines))
             (tags (plist-get x :tags))
             (links (plist-get x :links)))
        (when org-sanpo-debug (message "processing: %s" file))
        (when headlines (emacsql conn (vector :insert :into 'headlines :values headlines)))
        (when tags (emacsql conn (vector :insert :into 'tags :values tags)))
        (when links (emacsql conn (vector :insert :into 'links :values links)))))))

(defun org-sanpo--db-connect-or-create (db-file)
  (let ((conn (emacsql-sqlite db-file)))
    (emacsql conn "PRAGMA foreign_keys = ON")
    (unless (equal '((1)) (emacsql conn "PRAGMA foreign_keys"))
      (error "This SQlite version doesn't support foreign_keys"))
    conn))

;; 更新に備えて defvar ではなくて、defconst でなければならない。
(defconst org-sanpo--cache-schema-version 1)

(defconst org-sanpo--cache-schema
      '((headlines
         [(id text :not-null :primary-key)
          (file text :not-null)
          (title text :not-null)
          (level integer :not-null)
          (begin integer :not-null)
          (todo-keyword text)]
         (:foreign-key [file] :references files [file]
                       :on-delete :cascade))

        ;;
        (tags
         [(id text :not-null)
          (tag text :not-null)]
         (:foreign-key [id] :references headlines [id]
                       :on-delete :cascade))

        ;; 参照
        ;; src-nearest-id が nullable なのは toplevel や id の付いていない heading 内から参照される可能性があるため
        ;; src-nearest-id と src-nearest-title がどちらも値が設定されている可能性があるが、
        ;; その ID が headline のものとは限らない
        (links
         [(src-nearest-id text)
          (src-nearest-title text)
          (src-file text :not-null)
          (dist-id text :not-null)
          (link text :not-null)
          (link-begin integer :not-null)
          (context text :not-null)
          (context-begin integer :not-null)]
         (:foreign-key [src-file] :references files [file]
                       :on-delete :cascade))

        ;; file のパスと git の object-hash を格納
        (files
         [(file text :not-null :primary-key)
          (object-hash text :not-null)])

        ;; キャッシュのcommit
        ;; 一行のみ常に設定されている想定
        ;; head-commit はコミットツリー上のコミット。常に HEAD と同期する。
        ;; index-commit は index に追加された内容から先行して作成されるコミット
        ;; コミットツリー上にはまだ存在していない。
        ;; コミットしていない内容で cache に入るように。
        ;; head-commit を更新する際は nil に設定される。
        (commit
         [(commit text :not-null :primary-key)
          (index-commit text)])))


(provide 'org-sanpo)
