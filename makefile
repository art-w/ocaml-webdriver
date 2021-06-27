
.PHONY: test_lwt
test_lwt:
	dune exec --force test/test_lwt.exe

.PHONY: test_async
test_async:
	dune exec --force test/test_async.exe

.PHONY: selenium
selenium:
	cd vendor && ./run_selenium.sh

.PHONY: doc
doc: odoc.css
	rm -rf _doc
	dune build @doc
	cp -r _build/default/_doc _doc
	[ -f odoc.css ] && cp -f odoc.css _doc/_html/odoc.css

.PHONY: clean
clean:
	dune clean
	rm -rf _doc

.PHONY: width80
width80:
	find . -name '*.ml' | grep -v _build | xargs grep --color -E -e '^.{80,}| $$' \
		|| echo 'OK'

.PHONY: github_visit
github_visit:
	dune exec --force examples/github_visit.exe

.PHONY: google_search
google_search:
	dune exec --force examples/google_search.exe

