# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
The utils package contains tools used internally by pygccxml.

"""

from .utils import is_str
from .utils import native_compiler
from .utils import get_architecture
from .utils import loggers
from .utils import create_temp_file_name
from .utils import remove_file_no_raise
from .utils import normalize_path
from .utils import find_xml_generator
from .utils import get_tr1
from .utils import cxx_standard

# Version of xml generator which was used.
xml_generator = None
xml_output_version = None
