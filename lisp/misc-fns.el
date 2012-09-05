 (defun file-string (file &optional want-properties)
    "Read the contents of a file and return as a string."
    (with-temp-buffer
			(insert-file-contents-literally file)
			(buffer-string)))

