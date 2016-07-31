;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
					 '(("cls1" "class ${1:Name}\n{\npublic:\n    // Defalut constructor\n    ${1:$(yas/substr yas-text \"[^: ]*\")}() ${1:$(make-string (+ 6 (string-width (yas/substr yas-text \"[^: ]*\"))) ?\\ )} = default;\n    // Copy constructor\n    ${1:$(yas/substr yas-text \"[^: ]*\")}(const ${1:$(yas/substr yas-text \"[^: ]*\")}&) = default;\n    // Move constructor\n    ${1:$(yas/substr yas-text \"[^: ]*\")}(${1:$(yas/substr yas-text \"[^: ]*\")}&&)      = default;\n    // Destructor\n    ${2:~${1:$(yas/substr yas-text \"[^: ]*\")}()${1:$(make-string (+ 6 (string-width (yas/substr yas-text \"[^: ]*\"))) ?\\ )} = default;}\n    // Copy assignment operator\n    ${1:$(yas/substr yas-text \"[^: ]*\")}& operator=(const ${1:$(yas/substr yas-text \"[^: ]*\")}&) = default;\n    // Copy assignment operator\n    ${1:$(yas/substr yas-text \"[^: ]*\")}& operator=(${1:$(yas/substr yas-text \"[^: ]*\")}&)       = default;\n};\n$0\n" "class11" nil nil nil nil nil nil)
					   ("ns" "namespace ${1:Name}\n{\n$2\n}  // namespace: ${1:$(yas-substr yas-text \"[^: ]*\")}\n" "namespace { ... }" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu May  1 23:07:37 2014
