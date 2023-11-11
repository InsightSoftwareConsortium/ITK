# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

from datetime import date

project = 'ITK'
copyright = f'{date.today().year}, NumFOCUS'
author = 'Insight Software Consortium'

extensions = [
    'sphinx.ext.napoleon',
    'myst_parser',
    'sphinx.ext.intersphinx',
    'sphinx_copybutton',
    'sphinxext.opengraph',
    'sphinx_design',
]

myst_enable_extensions = [
    "colon_fence",
    "dollarmath",  # Support syntax for inline and block math using `$...$` and `$$...$$`
                   # (see https://myst-parser.readthedocs.io/en/latest/syntax/optional.html#dollar-delimited-math)
    "fieldlist",
    "linkify",  # convert bare links to hyperlinks
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


intersphinx_mapping = {
    "python": ("https://docs.python.org/3/", None),
    "numpy": ("https://numpy.org/doc/stable", None),
}

html_theme = 'furo'
html_static_path = ['_static']
# html_logo = "_static/itk-logo.svg"
html_logo = "_static/itk-logo-low-res.png"
html_favicon = "_static/icon/favicon.ico"
html_title = f"{project}'s documentation"

# Furo options
html_theme_options = {
    "top_of_page_button": "edit",
    "source_repository": "https://github.com/InsightSoftwareConsortium/ITK/",
    "source_branch": "master",
    "source_directory": "Documentation/docs",
}
