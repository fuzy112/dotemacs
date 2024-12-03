;; -*- coding: utf-8-unix; -*-
c-mode

(header "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-name-nondirectory (buffer-file-name)))) guard) n
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

c++-mode

(header "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-name-nondirectory (buffer-file-name)))) guard) n
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
