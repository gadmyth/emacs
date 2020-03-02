;;; package --- smart-compile-config.el
;;; Commentary:
;;; Code:

(require 'smart-compile)
(add-to-list 'smart-compile-alist '(objc-mode . "clang -fobjc-arc -framework Foundation -w %f && ./a.out"))

(defun objc-rewrite-cpp ()
  "."
  (interactive)
  (with-current-buffer (current-buffer)
    ;;; -mmacosx-version-min=10.7 -fobjc-runtime=macosx-10.7
    (let* ((ios-min-version "7.0")
           (runtime-version "ios-7.0")
           (sysroot "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
           (framework-path (concatenate 'string sysroot "/System/Library/Frameworks"))
           (library-path (concatenate 'string sysroot "/usr/lib"))
           (library-header-path (concatenate 'string sysroot "/usr/include"))
           (filename buffer-file-name)
           (command (format "clang -fobjc-arc -arch armv7 -framework Foundation -framework UIKit -rewrite-objc -miphoneos-version-min=%s -fobjc-runtime=%s -isysroot=%s -I\"%s\" -L\"%s\" -F\"%s\" -w %s"
                            ios-min-version
                            runtime-version
                            sysroot
                            library-header-path
                            library-path
                            framework-path
                            buffer-file-name))
           (result (shell-command-to-string command)))
      (message command)
      (message result))))

(provide 'smart-compile-config)
;;; smart-compile-config.el ends here