documentation: index.html Basic.html Array.html Vector.html Size.html Patch.html

%.html: %.lhs style.css
	pandoc -s -S --toc -c style.css -f markdown+lhs $< -o $@ --include-after=back.html

# Check that the tutorial can be loaded in GHCi
check:
	ghci *.lhs
