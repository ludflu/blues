build:
	cabal build

render:
	cabal run
	lilypond --pdf output.llp
