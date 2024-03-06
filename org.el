(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
          (org-mode . my/org-prettify-symbols))
  :commands (org-find-exact-headline-in-buffer org-set-tags)
  :custom-face
  ;; 设置Org mode标题以及每级标题行的大小
  (org-document-title ((t (:height 1.75 :weight bold))))
  (org-level-1 ((t (:height 1.2 :weight bold))))
  (org-level-2 ((t (:height 1.15 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold))))
  (org-level-4 ((t (:height 1.05 :weight bold))))
  (org-level-5 ((t (:height 1.0 :weight bold))))
  (org-level-6 ((t (:height 1.0 :weight bold))))
  (org-level-7 ((t (:height 1.0 :weight bold))))
  (org-level-8 ((t (:height 1.0 :weight bold))))
  (org-level-9 ((t (:height 1.0 :weight bold))))
  ;; 设置代码块用上下边线包裹
  (org-block-begin-line ((t (:underline t :background unspecified))))
  (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :config
  ;; ================================
  ;; 在org mode里美化字符串
  ;; ================================
  (defun my/org-prettify-symbols ()
    (setq prettify-symbols-alist
      (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
        '(
           ;; ("[ ]"              . 9744)         ; ☐
           ;; ("[X]"              . 9745)         ; ☑
           ;; ("[-]"              . 8863)         ; ⊟
           ("#+begin_src"      . 9998)         ; ✎
           ("#+end_src"        . 9633)         ; □
           ("#+begin_example"  . 129083)       ; 🠻
           ("#+end_example"    . 129081)       ; 🠹
           ("#+results:"       . 9776)         ; ☰
           ("#+attr_latex:"    . "🄛")
           ("#+attr_html:"     . "🄗")
           ("#+attr_org:"      . "🄞")
           ("#+name:"          . "🄝")         ; 127261
           ("#+caption:"       . "🄒")         ; 127250
           ("#+date:"          . "📅")         ; 128197
           ("#+author:"        . "💁")         ; 128100
           ("#+setupfile:"     . 128221)       ; 📝
           ("#+email:"         . 128231)       ; 📧
           ("#+startup:"       . 10034)        ; ✲
           ("#+options:"       . 9965)         ; ⛭
           ("#+title:"         . 10162)        ; ➲
           ("#+subtitle:"      . 11146)        ; ⮊
           ("#+downloaded:"    . 8650)         ; ⇊
           ("#+language:"      . 128441)       ; 🖹
           ("#+begin_quote"    . 187)          ; »
           ("#+end_quote"      . 171)          ; «
           ("#+begin_results"  . 8943)         ; ⋯
           ("#+end_results"    . 8943)         ; ⋯
           )))
    (setq prettify-symbols-unprettify-at-point t)
    (prettify-symbols-mode 1))

  ;; 提升latex预览的图片清晰度
  (plist-put org-format-latex-options :scale 1.8)

  ;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
  (setq org-blank-before-new-entry '((heading . t)
                                      (plain-list-item . auto)
                                      ))

  ;; ======================================
  ;; 设置打开Org links的程序
  ;; ======================================
  (defun my-func/open-and-play-gif-image (file &optional link)
    "Open and play GIF image `FILE' in Emacs buffer.

Optional for Org-mode file: `LINK'."
    (let ((gif-image (create-image file))
           (tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
      (switch-to-buffer tmp-buf)
      (erase-buffer)
      (insert-image gif-image)
      (image-animate gif-image nil t)
      (local-set-key (kbd "q") 'bury-buffer)
      ))
  (setq org-file-apps '(("\\.png\\'"     . default)
                         (auto-mode       . emacs)
                         (directory       . emacs)
                         ("\\.mm\\'"      . default)
                         ("\\.x?html?\\'" . default)
                         ("\\.pdf\\'"     . emacs)
                         ("\\.md\\'"      . emacs)
                         ("\\.gif\\'"     . my-func/open-and-play-gif-image)
                         ("\\.xlsx\\'"    . default)
                         ("\\.svg\\'"     . default)
                         ("\\.pptx\\'"    . default)
                         ("\\.docx\\'"    . default)))

  :custom
  ;; 设置Org mode的目录
  (org-directory "~/org")
  ;; 设置笔记的默认存储位置
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; 启用一些子模块
  (org-modules '(ol-bibtex ol-gnus ol-info ol-eww org-habit org-protocol))
  ;; 在按M-RET时，是否根据光标所在的位置分行，这里设置为是
  ;; (org-M-RET-may-split-line '((default . nil)))
  ;; 一些Org mode自带的美化设置
  ;; 标题行美化
  (org-fontify-whole-heading-line t)
  ;; 设置标题行折叠符号
  (org-ellipsis " ▾")
  ;; 在活动区域内的所有标题栏执行某些命令
  (org-loop-over-headlines-in-active-region t)
  ;; TODO标签美化
  (org-fontify-todo-headline t)
  ;; DONE标签美化
  (org-fontify-done-headline t)
  ;; 引用块美化
  (org-fontify-quote-and-verse-blocks t)
  ;; 隐藏宏标记
  (org-hide-macro-markers t)
  ;; 隐藏强调标签
  (org-hide-emphasis-markers t)
  ;; 高亮latex语法
  (org-highlight-latex-and-related '(native script entities))
  ;; 以UTF-8显示
  (org-pretty-entities t)
  ;; 是否隐藏标题栏的前置星号，这里我们通过org-modern来隐藏
  ;; (org-hide-leading-stars t)
  ;; 当启用缩进模式时自动隐藏前置星号
  (org-indent-mode-turns-on-hiding-stars t)
  ;; 自动启用缩进
  (org-startup-indented nil)
  ;; 根据标题栏自动缩进文本
  (org-adapt-indentation nil)
  ;; 自动显示图片
  (org-startup-with-inline-images t)
  ;; 默认以Overview的模式展示标题行
  (org-startup-folded 'overview)
  ;; 允许字母列表
  (org-list-allow-alphabetical t)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(
                                    ("-"  . "+")
                                    ("+"  . "1.")
                                    ("1." . "a.")
                                    ))
  ;; 编辑时检查是否在折叠的不可见区域
  (org-fold-catch-invisible-edits 'smart)
  ;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
  (org-insert-heading-respect-content nil)
  ;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
  ;; 四种设置方法：(1080), 1080, t, nil
  (org-image-actual-width nil)
  ;; imenu的最大深度，默认为2
  (org-imenu-depth 4)
  ;; 回车要不要触发链接，这里设置不触发
  (org-return-follows-link nil)
  ;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
  (org-use-sub-superscripts '{})
  ;; 复制粘贴标题行的时候删除id
  (org-clone-delete-id t)
  ;; 粘贴时调整标题行的级别
  (org-yank-adjusted-subtrees t)

  ;; TOOD的关键词设置，可以设置不同的组
  (org-todo-keywords '((sequence "TODO(t)" "HOLD(h!)" "WIP(i!)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f!)")))
  ;; TODO关键词的样式设置
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                             ("HOLD"       :foreground "#feb24c" :weight bold)
                             ("WIP"        :foreground "#0098dd" :weight bold)
                             ("WAIT"       :foreground "#9f7efe" :weight bold)
                             ("DONE"       :foreground "#50a14f" :weight bold)
                             ("CANCELLED"  :foreground "#ff6480" :weight bold)
                             ("REPORT"     :foreground "magenta" :weight bold)
                             ("BUG"        :foreground "red"     :weight bold)
                             ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
                             ("FIXED"      :foreground "green"   :weight bold)))
  ;; 当标题行状态变化时标签同步发生的变化
  ;; Moving a task to CANCELLED adds a CANCELLED tag
  ;; Moving a task to WAIT adds a WAIT tag
  ;; Moving a task to HOLD adds WAIT and HOLD tags
  ;; Moving a task to a done state removes WAIT and HOLD tags
  ;; Moving a task to TODO removes WAIT, CANCELLED, and HOLD tags
  ;; Moving a task to DONE removes WAIT, CANCELLED, and HOLD tags
  (org-todo-state-tags-triggers
    (quote (("CANCELLED" ("CANCELLED" . t))
             ("WAIT" ("WAIT" . t))
             ("HOLD" ("WAIT") ("HOLD" . t))
             (done ("WAIT") ("HOLD"))
             ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
             ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))
  ;; 使用专家模式选择标题栏状态
  (org-use-fast-todo-selection 'expert)
  ;; 父子标题栏状态有依赖
  (org-enforce-todo-dependencies t)
  ;; 标题栏和任务复选框有依赖
  (org-enforce-todo-checkbox-dependencies t)
  ;; 优先级样式设置
  (org-priority-faces '((?A :foreground "red")
                         (?B :foreground "orange")
                         (?C :foreground "yellow")))
  ;; 标题行全局属性设置
  (org-global-properties '(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
                            ("APPT_WARNTIME_ALL" . "0 5 10 15 20 25 30 45 60")
                            ("RISK_ALL" . "Low Medium High")
                            ("STYLE_ALL" . "habit")))
  ;; Org columns的默认格式
  (org-columns-default-format "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  ;; 当状态从DONE改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; DONE时加上时间戳
  (org-log-done 'time)
  ;; 重复执行时加上时间戳
  (org-log-repeat 'time)
  ;; Deadline修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;; refile使用缓存
  (org-refile-use-cache t)
  ;; refile的目的地，这里设置的是agenda文件的所有标题
  (org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
  ;; 将文件名加入到路径
  (org-refile-use-outline-path 'file)
  ;; 是否按步骤refile
  (org-outline-path-complete-in-steps nil)
  ;; 允许创建新的标题行，但需要确认
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; 设置标签的默认位置，默认是第77列右对齐
  ;; (org-tags-column -77)
  ;; 自动对齐标签
  (org-auto-align-tags t)
  ;; 标签不继承
  (org-use-tag-inheritance nil)
  ;; 在日程视图的标签不继承
  (org-agenda-use-tag-inheritance nil)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 OREDERD 标签
  (org-track-ordered-property-with-tag t)
  ;; 始终存在的的标签
  (org-tag-persistent-alist '(("read"     . ?r)
                               ("mail"     . ?m)
                               ("emacs"    . ?e)
                               ("study"    . ?s)
                               ("work"     . ?w)))
  ;; 预定义好的标签
  (org-tag-alist '((:startgroup)
                    ("crypt"    . ?c)
                    ("linux"    . ?l)
                    ("apple"    . ?a)
                    ("noexport" . ?n)
                    ("ignore"   . ?i)
                    ("TOC"      . ?t)
                    (:endgroup)))

  ;; 归档设置
  (org-archive-location "%s_archive::datetree/")
  )

;; Org mode的附加包，有诸多附加功能
;; (use-package org-contrib
;;   :ensure t)


(use-package org-modern
  :ensure t
  :hook (after-init . (lambda ()
                        (setq org-modern-hide-stars 'leading)
                        (global-org-modern-mode t)))
  :config
  ;; 标题行型号字符
  (setq org-modern-star ["◉" "○" "✸" "✳" "◈" "◇" "✿" "❀" "✜"])
  ;; 额外的行间距，0.1表示10%，1表示1px
  (setq-default line-spacing 0.1)
  ;; tag边框宽度，还可以设置为 `auto' 即自动计算
  (setq org-modern-label-border 1)
  ;; 设置表格竖线宽度，默认为3
  (setq org-modern-table-vertical 2)
  ;; 设置表格横线为0，默认为0.1
  (setq org-modern-table-horizontal 0)
  ;; 复选框美化
  (setq org-modern-checkbox
    '((?X . #("▢✓" 0 2 (composition ((2)))))
       (?- . #("▢–" 0 2 (composition ((2)))))
       (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
    '((?- . "•")
       (?+ . "◦")
       (?* . "▹")))
  ;; 代码块左边加上一条竖边线（需要Org mode顶头，如果启用了 `visual-fill-column-mode' 会很难看）
  (setq org-modern-block-fringe t)
  ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-block-name nil)
  ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-keyword nil)
  )

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :bind (("\e\e a" . org-agenda)
          :map org-agenda-mode-map
          ("i" . (lambda () (interactive) (org-capture nil "d")))
          ("J" . consult-org-agenda))
  :config
  ;; 日程模式的日期格式设置
  ;;  (setq org-agenda-format-date 'org-agenda-format-date-aligned)
  ;;   (defun org-agenda-format-date-aligned (date)
  ;;     "Format a DATE string for display in the daily/weekly agenda, or timeline.

  ;; This function makes sure that dates are aligned for easy reading."
  ;;     (require 'cal-iso)
  ;;     (let* ((dayname (aref cal-china-x-days
  ;;                       (calendar-day-of-week date)))
  ;;             (day (cadr date))
  ;;             (month (car date))
  ;;             (year (nth 2 date))
  ;;             (day-of-week (calendar-day-of-week date))
  ;;             (iso-week (org-days-to-iso-week
  ;;                         (calendar-absolute-from-gregorian date)))
  ;;             (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
  ;;             (cn-month (cl-caddr cn-date))
  ;;             (cn-day (cl-cadddr cn-date))
  ;;             (cn-month-string (concat (aref cal-china-x-month-name
  ;;                                        (1- (floor cn-month)))
  ;;                                (if (integerp cn-month)
  ;;                                  ""
  ;;                                  "（闰月）")))
  ;;             (cn-day-string (aref cal-china-x-day-name
  ;;                              (1- cn-day)))
  ;;             (extra (format " 农历%s%s%s%s"
  ;;                      (if (or (eq org-agenda-current-span 'day)
  ;;                            (= day-of-week 1)
  ;;                            (= cn-day 1))
  ;;                        cn-month-string
  ;;                        "")
  ;;                      (if (or (= day-of-week 1)
  ;;                            (= cn-day 1))
  ;;                        (if (integerp cn-month) "" "[闰]")
  ;;                        "")
  ;;                      cn-day-string
  ;;                      (if (or (= day-of-week 1)
  ;;                            (eq org-agenda-current-span 'day))
  ;;                        (format " 今年第%02d周" iso-week)
  ;;                        "")
  ;;                      ))
  ;;             )
  ;;       (format "%04d-%02d-%02d 星期%s%s%s\n" year month
  ;;         day dayname extra (concat " 第" (format-time-string "%j") "天"))))

  ;; 显示时间线
  (setq org-agenda-use-time-grid t)
  ;; 设置面包屑分隔符
  ;; (setq org-agenda-breadcrumbs-separator " ❱ ")
  ;; 设置时间线的当前时间指示串
  (setq org-agenda-current-time-string "⏰------------now")
  ;; 时间线范围和颗粒度设置
  (setq org-agenda-time-grid (quote ((daily today)
                                      (0600 0800 1000 1200
                                        1400 1600 1800
                                        2000 2200 2400)
                                      "......" "----------------")))
  ;; 日程视图的前缀设置
  (setq org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                                    (todo   . " %i %-25:c ")
                                    (tags   . " %i %-25:c ")
                                    (search . " %i %-25:c ")))
  ;; 对于计划中的任务在视图里的显示
  (setq org-agenda-scheduled-leaders
    '("计划 " "应在%02d天前开始 "))
  ;; 对于截止日期的任务在视图里的显示
  (setq org-agenda-deadline-leaders
    '("截止 " "还有%02d天到期 " "已经过期%02d天 "))

  ;; =====================
  ;; 自定义日程视图，分别显示TODO，WIP，WIAT中的任务
  ;; n键显示自定义视图，p键纯文本视图，a键默认视图
  ;; =====================
  ;; (defvar my-org-custom-daily-agenda
  ;;   `((todo "TODO"
  ;;       ((org-agenda-block-separator nil)
  ;;         (org-agenda-overriding-header "所有待办任务\n")))
  ;;      (todo "WIP"
  ;;        ((org-agenda-block-separator nil)
  ;;          (org-agenda-overriding-header "\n进行中的任务\n")))
  ;;      (todo "WAIT"
  ;;        ((org-agenda-block-separator nil)
  ;;          (org-agenda-overriding-header "\n等待中的任务\n")))
  ;;      (agenda "" ((org-agenda-block-separator nil)
  ;;                   (org-agenda-overriding-header "\n今日日程\n"))))
  ;;   "Custom agenda for use in `org-agenda-custom-commands'.")
  ;; (setq org-agenda-custom-commands
  ;;   `(("n" "Daily agenda and top priority tasks"
  ;;       ,my-org-custom-daily-agenda)
  ;;      ("p" "Plain text daily agenda and top priorities"
  ;;        ,my-org-custom-daily-agenda
  ;;        ((org-agenda-with-colors nil)
  ;;          (org-agenda-prefix-format "%t %s")
  ;;          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
  ;;          (org-agenda-fontify-priorities nil)
  ;;          (org-agenda-remove-tags t))
  ;;        ("agenda.txt"))))

  ;; 时间戳格式设置，会影响到 `svg-tag' 等基于正则的设置
  ;; 这里设置完后是 <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
  (setq system-time-locale "zh_CN.UTF-8")
  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 不同日程类别间的间隔
  (setq org-cycle-separator-lines 2)
  :custom
  ;; 设置需要被日程监控的org文件
  (org-agenda-files
    (list (expand-file-name "tasks.org" org-directory)
      (expand-file-name "diary.org" org-directory)
      (expand-file-name "habits.org" org-directory)
      (expand-file-name "mail.org" org-directory)
      (expand-file-name "emacs-config.org" user-emacs-directory)
      ))
  ;; 设置org的日记文件
  (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  ;; 日记插入精确时间戳
  (org-agenda-insert-diary-extract-time t)
  ;; 设置日程视图更加紧凑
  ;; (org-agenda-compact-blocks t)
  ;; 日程视图的块分隔符
  (org-agenda-block-separator ?─)
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
  (org-agenda-span 'day)
  ;; q退出时删除agenda缓冲区
  (org-agenda-sticky t)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 禁止日程启动画面
  (org-agenda-inhibit-startup t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动log mode
  (org-agenda-start-with-log-mode t)
  ;; 日程同时启动任务时间记录报告模式
  (org-agenda-start-with-clockreport-mode t)
  ;; 截止的任务完成后不显示
  ;; (org-agenda-skip-deadline-if-done t)
  ;; 当计划的任务完成后不显示
  (org-agenda-skip-scheduled-if-done t)
  ;; 计划过期上限
  (org-scheduled-past-days 365)
  ;; 计划截止上限
  (org-deadline-past-days 365)
  ;; 计划中的任务不提醒截止时间
  (org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; 设置工时记录报告格式
  (org-agenda-clockreport-parameter-plist
    '(:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; 标签显示的位置，第100列往前右对齐
  (org-agenda-tags-column -100)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 是否使用am/pm
  ;; (org-agenda-timegrid-use-ampm nil)
  ;; 搜索是不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前3天截止日期到期告警
  (org-deadline-warning-days 3)
  )

(use-package org-capture
  :ensure nil
  :bind ("\e\e c" . (lambda () (interactive) (org-capture)))
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
          (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                             "* TODO %i%?"
                             :empty-lines-after 1
                             :prepend t)
                            ("n" "Notes" entry (file+headline "capture.org" "Notes")
                              "* %? %^g\n%i\n"
                              :empty-lines-after 1)
                            ;; For EWW
                            ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                              "* %:description\n\n%a%?"
                              :empty-lines 1
                              :immediate-finish t)
                            ("d" "Diary")
                            ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                              "* Today's TODO list [/]\n%T\n\n** TODO %?"
                              :empty-lines 1
                              :jump-to-captured t)
                            ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                              "* %?\n%T\n\n%i"
                              :empty-lines 1
                              :jump-to-captured t)
                            ))
  )

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :bind (("C-c d n" . denote)
          ("C-c d d" . denote-date)
          ("C-c d t" . denote-type)
          ("C-c d s" . denote-subdirectory)
          ("C-c d f" . denote-open-or-create)
          ("C-c d r" . denote-dired-rename-file))
  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
      '("N" "New note (with denote.el)" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)))
  :config
  (setq denote-directory (expand-file-name "~/org/"))
  (setq denote-known-keywords '("emacs" "entertainment" "reading" "studying"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; org is default, set others such as text, markdown-yaml, markdown-toml
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords))

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)
  (setq denote-date-format nil)

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (setq denote-dired-rename-expert nil)

  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  )
