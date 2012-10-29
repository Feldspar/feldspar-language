documentation: index.html Basic.html Arrays.html Vector.html SizeConstraints.html

%.html: %.lhs style.css
	pandoc -s -S --toc -c style.css -f markdown+lhs $< -o $@ --include-after=back.html
