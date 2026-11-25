# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/main/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/main/usage/configuration.html#project-information

import os
import sys
from datetime import date
from pathlib import Path

# Generates migration_guides/generated/ from the migration records.
sys.path.insert(
    0, str(Path(__file__).resolve().parents[2] / "Utilities" / "Maintenance")
)

project = "ITK"
copyright = f"{date.today().year}, NumFOCUS"
author = "Insight Software Consortium"

# Define the canonical URL if you are using a custom domain on Read the Docs
html_baseurl = os.environ.get("READTHEDOCS_CANONICAL_URL", "docs.itk.org")

html_context = {}
# Tell Jinja2 templates the build is running on Read the Docs
if os.environ.get("READTHEDOCS", "") == "True":
    html_context["READTHEDOCS"] = True

extensions = [
    "sphinx.ext.napoleon",
    "myst_parser",
    "sphinx.ext.intersphinx",
    "sphinx_copybutton",
    "sphinxext.opengraph",
    "sphinx_design",
    "MigrationRecords",
]

myst_enable_extensions = [
    "colon_fence",
    "dollarmath",  # Support syntax for inline and block math using `$...$` and `$$...$$`
    # (see https://myst-parser.readthedocs.io/en/latest/syntax/optional.html#dollar-delimited-math)
    "fieldlist",
    "linkify",  # convert bare links to hyperlinks
]

templates_path = ["_templates"]
exclude_patterns = [
    "_build",
    "Thumbs.db",
    ".DS_Store",
    # Frozen hand-written guides, replaced by migration_guides/generated/.
    "migration_guides/itk_*_migration_guide*.md",
    "migration_guides/python_spatial_key_migration.md",
    "migration_guides/joint_histogram_mutual_information_metric_correction.md",
]


intersphinx_mapping = {
    "python": ("https://docs.python.org/3/", None),
    "numpy": ("https://numpy.org/doc/stable", None),
}

html_theme = "furo"
html_static_path = ["_static"]
# html_logo = "_static/itk-logo.svg"
html_logo = "_static/itk-logo-low-res.png"
html_favicon = "_static/icon/favicon.ico"
html_title = f"{project}'s documentation"

# Furo options
html_theme_options = {
    "top_of_page_button": "edit",
    "source_repository": "https://github.com/InsightSoftwareConsortium/ITK/",
    "source_branch": "main",
    "source_directory": "Documentation/docs",
}
