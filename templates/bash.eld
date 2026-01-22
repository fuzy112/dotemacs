sh-mode

(defrequo (p "regexp_quote" name) "() {" n>
	  "printf \"%s\\n\" \"${1//[.*^\\$\\\\\\[\\]]/\\\\&}\"" n
	  "}" n)

(sourcedir "SOURCE_DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]:-$0}\" )\" &> /dev/null && pwd )\"")
