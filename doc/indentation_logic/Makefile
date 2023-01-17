pages.pdf: pages/page_000.pdf
	pdfunite pages/page_*.pdf pages.pdf

pages/page_000.pdf: pages/page_000.svg
	for i in pages/page_*.svg ; do inkscape --export-pdf=pages/"$$( basename "$${i}" .svg )".pdf "$${i}" ; done

pages/page_000.svg: src/pages.svg src/split_pages.kts
	kotlinc-jvm -script src/split_pages.kts src/pages.svg

clean:
	rm -rf pages.pdf pages
