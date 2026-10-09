.PHONY: restore analysis figures tables manuscript paper readme format lint test check ci-docker clean

restore:
	Rscript -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(prompt = FALSE)'

analysis:
	Rscript scripts/run_all.R

figures: analysis
	Rscript scripts/figures.R
	pdftoppm -singlefile -r 150 -png figs/experiment_effects.pdf figs/experiment_effects

tables: analysis
	Rscript scripts/tables.R

manuscript:
	cd ms && latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

readme: analysis
	Rscript -e 'knitr::knit("README.Rmd", output = "README.md", quiet = TRUE)'

paper: figures tables readme
	$(MAKE) manuscript

format:
	Rscript -e 'for (d in c("R", "scripts", "tests")) styler::style_dir(d)'

lint:
	Rscript -e 'for (f in list.files("R", pattern = "[.]R$$", full.names = TRUE)) source(f); l <- unlist(lapply(c("R", "scripts", "tests"), lintr::lint_dir), recursive = FALSE); print(l); quit(status = as.integer(length(l) > 0))'

test:
	Rscript -e 'testthat::test_dir("tests/testthat", stop_on_failure = TRUE)'

check: paper lint test

ci-docker:
	docker run --rm -e RENV_PATHS_CACHE=/project/renv/cache -v "$(CURDIR):/project" -w /project rocker/r-ver:4.6.0 \
		bash -lc 'apt-get update && apt-get install -y --no-install-recommends make libxml2-dev libuv1-dev poppler-utils latexmk texlive-xetex texlive-latex-extra texlive-fonts-recommended lmodern && make restore check'

clean:
	cd ms && latexmk -C main.tex
