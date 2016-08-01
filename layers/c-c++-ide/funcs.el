;; Additional C++11 keywords
;;  <http://stackoverflow.com/questions/8549351/c11-mode-or-settings-for-emacs>

(defun c-c++-ide-cc-mode--copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))
;; labels, case, public, private, proteced, namespace-tags
(c-c++-ide-cc-mode--copy-face 'font-lock-label-face      'font-lock-keyword-face)
;; comment markups such as Javadoc-tags
(c-c++-ide-cc-mode--copy-face 'font-lock-doc-markup-face 'font-lock-doc-face)
;; comment markups
(c-c++-ide-cc-mode--copy-face 'font-lock-doc-string-face 'font-lock-comment-face)

(defun c-c++-ide-cc-mode--add-keywords-for-c++11 ()
  (font-lock-add-keywords
   nil '(;; complete some fundamental keywords
         ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
         ;; namespace names and tags - these are rendered as constants by cc-mode
         ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
         ;;  new C++11 keywords
         ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
         ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
         ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
         ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
         ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
         ;; hexadecimal numbers
         ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
         ;; integer/float/scientific numbers
         ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
         ;; c++11 string literals
         ;;       L"wide string"
         ;;       L"wide string with UNICODE codepoint: \u2018"
         ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
         ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
         ;;       R"(user-defined literal)"
         ;;       R"( a "quot'd" string )"
         ;;       R"delimiter(The String Data" )delimiter"
         ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"
         ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
         (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\(.*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string
         (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(.*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

         ;; user-defined types (rather project-specific)
         ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
         ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
         )
   t))
