<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY html-ss PUBLIC
  "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY html-split-ss PUBLIC
  "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY print-ss PUBLIC
  "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
]>

<style-sheet>
  
  <style-specification id="html" use="html-stylesheet">
    
    <style-specification-body>

;; Stylesheet fuer einteilige HTML-Version

      (define ($generate-qandaset-toc$) #f)
      (define nochunks #t)
      (define %link-mailto-url% "mailto:dev@openxp.de")
      
    </style-specification-body>
  
  </style-specification>

  <style-specification id="html-split" use="html-split-stylesheet">
    
    <style-specification-body>

;; Stylesheet fuer aufgesplittete HTML-Version (fuer's Web)

      (define %root-filename% "index")
      (define html-manifest #t)
      (define use-output-dir #t)
      (define %output-dir% "htmlsplt")
      (define %use-id-as-filename% #t)
      (define %link-mailto-url% "mailto:dev@openxp.de")
      
    </style-specification-body>
  
  </style-specification>

  <style-specification id="print" use="print-stylesheet">
    
    <style-specification-body>

;; Stylesheet fuer RTF- und TeX-Version

    </style-specification-body>
  
  </style-specification>

  <external-specification id="html-stylesheet" document="html-ss">

  <external-specification id="html-split-stylesheet"
    document="html-split-ss">

  <external-specification id="print-stylesheet" document="print-ss">

</style-sheet>
