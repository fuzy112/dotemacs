;; -*- coding: utf-8-unix; -*-
c-mode

(header "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-name-nondirectory (buffer-file-name)))) guard) n
	"#define " guard r n n
	"#endif " comment-start "!" guard comment-end)

(pheader "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-relative-name (buffer-file-name) (if-let* ((project (project-current))) (project-root project) default-directory)))) guard) n
	"#define " guard r n n
	"#endif " comment-start "!" guard comment-end)

(options "static const struct option " (p "options") "[] = " "{" n>
	 "{ \"help\", no_argument, NULL, 'h' }," n>
	 "{ \"verbose\", no_argument, NULL, 'v' }," n>
	 "{ 0, 0, 0, 0 }," n
	 "};")

(getopt "while ((" (p "opt" opt) " = getopt(argc, argv, " (p "\"hv\"" shortopts) ")) != -1) {" n>
	"switch (" opt ") {" n>
	"case 'h':" n>
	"/* todo */" n>
	"break;" n>
	"case 'v':" n>
	"/* todo */" n>
	"break;" n>
	"default:" n>
	"/* todo */" n>
	"break;" n>
	"}" n>
	"}" n)

(getopt_long "while ((" (p "c" opt) " = getopt(argc, argv, " (p "\"hv\"" short-options) ", "(p "long_options" long-options)", &" (p "option_index" option-index) ")) != -1) {" n>
	     "  switch (" opt ") {" n>
	     "  case 'h':" n>
	     "    /* todo */" n>
	     "    break;" n>
	     "  case 'v':" n>
	     "    /* todo */" n>
	     "    break;" n>
	     "  default:" n>
	     "    /* todo */" n>
	     "    break;" n>
	     "  }" n>
	     "}" n)

(_state "struct " (p "name" name) "_state {" n>
	n
	"};" n)

(_send "struct tevent_req *" (p "name" name) "_send(TALLOC_CTX *mem_ctx, struct tevent_context *ev)" n
       "{" n>
       "struct tevent_req *req;" n>
       "struct " name "_state *state = NULL;" n>
       "errno_t ret = 0;" n n>
       "req = tevent_req_create(mem_ctx, &state, struct " name "_state);" n>
       "if (req == NULL)" n>
       "return NULL;" n n>
       "return req;" n n
       "immediately:" n>
       "if (ret == 0)" n>
       "tevent_req_done(req);" n>
       "else" n>
       "tevent_req_error(req, ret);" n>
       "tevent_req_post(req, ev);" n>
       "return req;" n
       "}")

(_done "static void " (p "name" name) "_done(struct tevent_req *subreq)" n
       "{" n>
       "struct tevent_req *req;" n>
       "struct " name "_state *state;" n>
       "errno_t ret = 0;" n n>
       "req = tevent_req_callback_data(subreq, struct tevent_req);" n>
       "state = tevent_req_data(req, struct " name "_state);" n> n
       "done:" n>
       "if (ret == 0)" n>
       "tevent_req_done(req);" n>
       "else" n>
       "tevent_req_error(req, ret);" n
       "}" n)

c++-mode

(header "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-name-nondirectory (buffer-file-name)))) guard) n
	"#define " guard r n n
	"#endif " comment-start "!" guard comment-end)

(pheader "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-relative-name (buffer-file-name) (if-let* ((project (project-current))) (project-root project) default-directory)))) guard) n
	"#define " guard r n n
	"#endif " comment-start "!" guard comment-end)

(qobject "class "  (p "Object" name)  " : public " (p "QObject" base) "{" n>
	 "Q_OBJECT" n
	 "public:" n>
	 "explicit " name "(QObject *parent = nullptr);" n
	 "};" n)

(qwidget "class " (p "Widget" name) " : public " (p "QWidget" base) " {" n>
	 "Q_OBJECT" n
	 "public:" n>
	 "explicit " name "(QWidget *parent = nullptr);" n
	 "};" n)
