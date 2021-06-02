# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import sys
import os
from subprocess import Popen, PIPE

sys.path.insert(0, os.path.abspath(os.path.join("..", "doc")))

# -- determine if running on readthedocs ------------------------------------
on_rtd = os.environ.get('READTHEDOCS') == 'True'

# -- Update the modflow 6 version -------------------------------------------
print("Update the modflow6 version")
pth = os.path.join("..", "distribution")
args = (
    "python",
    "make_release.py",
)
# run the command
proc = Popen(args, stdout=PIPE, stderr=PIPE, cwd=pth)
stdout, stderr = proc.communicate()
if stdout:
    print(stdout.decode("utf-8"))
if stderr:
    print("Errors:\n{}".format(stderr.decode("utf-8")))

# -- import version from doc/version.py -------------------------------------
from version import __version__

# -- build the mf6io markdown files -----------------------------------------
print("Build the mf6io markdown files")
pth = os.path.join("..", "doc", "mf6io", "mf6ivar")
args = (
    "python",
    "mf6ivar.py"
)
# run the command
proc = Popen(args, stdout=PIPE, stderr=PIPE, cwd=pth)
stdout, stderr = proc.communicate()
if stdout:
    print(stdout.decode("utf-8"))
if stderr:
    print("Errors:\n{}".format(stderr.decode("utf-8")))

# -- Project information -----------------------------------------------------

project = "MODFLOW 6 Program Documentation"
copyright = "2020, MODFLOW Development Team"
author = "MODFLOW Development Team"

# -- Project version ---------------------------------------------------------
version = __version__
release = __version__

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx.ext.napoleon",
    "sphinx.ext.doctest",
    "sphinx.ext.intersphinx",
    "sphinx.ext.todo",
    "sphinx.ext.coverage",
    "sphinx.ext.mathjax",
    "sphinx.ext.ifconfig",
    "sphinx.ext.viewcode",
    # "sphinx.ext.graphviz",
    "IPython.sphinxext.ipython_console_highlighting",  # lowercase didn't work
    "sphinx.ext.autosectionlabel",
    "nbsphinx",
    "nbsphinx_link",
    "recommonmark",
    "sphinx_markdown_tables",
    "breathe",
    "exhale",
]

source_suffix = {
    '.rst': 'restructuredtext',
    '.md': 'markdown',
}

# Breathe Configuration
breathe_default_project = "mf6src"

breathe_projects = {
    "mf6src": "./xml/",
}

# Doxygen string
doxy_str = (
    # directories to include in doxygen
    "INPUT = ",
    "../srcbmi/ ",
    "../src/",
    # add other doxygen comments
    "STRIP_CODE_COMMENTS = NO",
    "CLASS_DIAGRAMS = YES",
    # "HAVE_DOT = YES",
    # "INCLUDE_GRAPH = YES",
    # "INCLUDED_BY_GRAPH = YES",
    # "CALL_GRAPH = YES",
    # "CALLER_GRAPH = YES",
    # "GRAPHICAL_HIERARCHY = YES",
    # "DIRECTORY_GRAPH = YES",
    "OPTIMIZE_FOR_FORTRAN = YES",
    "EXTRACT_ALL = YES",
)

# Setup the exhale extension
exhale_args = {
    # These arguments are required
    "containmentFolder": "./_mf6src",
    "rootFileName": "mf6src.rst",
    "rootFileTitle": "MODFLOW 6 Source Code",
    "doxygenStripFromPath": "..",
    # Suggested optional arguments
    "createTreeView": True,
    # TIP: if using the sphinx-bootstrap-theme, you need
    # "treeViewIsBootstrap": True,
    "exhaleExecutesDoxygen": True,
    # "exhaleUseDoxyfile": True,
    "exhaleDoxygenStdin": " ".join(doxy_str)
}

# Tell sphinx what the primary language being documented is.
primary_domain = 'fortran'

# Tell sphinx what the pygments highlight language should be.
highlight_language = 'fortran'

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = "sphinx_rtd_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

html_context = {
    'css_files': [
        '_static/theme_overrides.css',  # override wide tables in RTD theme
    ],
}

html_theme_options = {
    "github_url": "https://github.com/MODFLOW-USGS/modflow6",
    "use_edit_page_button": False
}

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
html_use_smartypants = True

# If false, no module index is generated.
# html_domain_indices = True

# If false, no index is generated.
# html_use_index = True

# If true, the index is split into individual pages for each letter.
# html_split_index = False

# If true, links to the reST sources are added to the pages.
# html_show_sourcelink = True

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
html_show_sphinx = True
# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
html_show_copyright = True
