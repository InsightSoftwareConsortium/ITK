# Contribute documentation improvements

Thank you for improving our documentation! Every addition made is read
thousands of times and has a tremendous impact.

These are the sources for the [ITK docs](https://docs.itk.org)
[sphinx](https://www.sphinx-doc.org/) website.

The contribution process generally follows our [contributing
guidelines](./contributing/index.md).

Once merged into `master`, the documentation will be available in the `latest`
version of the ReadTheDocs documentation as https://docs.itk.org/. Note that
`latest` is not the default version (`release` is). To change versions, use the links in the lower
left of the page.

To preview documentation changes, three options are available.

## Option 0: Edit on GitHub, use the pull request preview

If files are edited with GitHub's web user interface, the pull request will
build a preview of changes with a pull request check called `docs/readthedocs.org:itk`.
Click on the *Details* link to view the documentation build preview.

## Option 1: Build and serve locally

To compile the document locally create a python virtual environment and install the required packages.

For example in Linux / macOS:

```bash
cd ITK/Documentation/docs
python -m venv env
source env/bin/activate
pip install -r requirements.txt
```

Use `make html` in this directory to build the documentation.
Open `_build/html/index.html` in your browser to inspect the result.

## Option 2: Autobuild and serve locally

To automatically rebuild the website with any input markdown changes and serve
the result, use [sphinx-autobuild]

```bash
cd ITK/Documentation/docs
pip install -r requirements.txt
pip install sphinx-autobuild
```

```
sphinx-autobuild -a . _build/html
```

This will start a server at [http://127.0.0.1:8000](http://127.0.0.1:8000)
and rebuild whenever the documentation changes.

[sphinx-autobuild]: https://github.com/executablebooks/sphinx-autobuild
