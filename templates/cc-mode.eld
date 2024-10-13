c-mode

(header "#ifndef " (p (upcase (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" (file-name-nondirectory (buffer-file-name)))) guard) n
	"#define " guard r n n
	"#endif " comment-start "!" guard comment-end)

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
