#!/usr/bin/env python
# \author Hans J. Johnson
# The following python script was used to make the ITK code consistent.

# This program is designed to replace the use of "class" with
# the more expressive "typename" keyword for defining template
# parameters

# Usage: python ReplaceClassWithTypenameKeyword.py *.hxx *.h *.cxx

import re
import sys


def replaceOneInstance(desired_outs):
    templateSearchPattern = r"\S*template *<([^>]*?\bclass\b[^>]*?)>"
    compiledSearchPattern = re.compile(templateSearchPattern, re.MULTILINE | re.DOTALL)
    foundMatch = compiledSearchPattern.search(desired_outs)
    if not foundMatch:
        return desired_outs  ##Break recursion
    else:
        oldguts = foundMatch.groups(0)[0]
        replacePattern = re.compile(r"\bclass\b")
        newguts = replacePattern.sub(r"typename", oldguts)
        desired_outs = desired_outs.replace(oldguts, newguts)
        desired_outs = replaceOneInstance(desired_outs)
    return desired_outs


for fileName in sys.argv[1:]:
    print(f"Processing {fileName}")
    filePtr = open(fileName)
    fileOrignalText = filePtr.read()
    filePtr.close()

    fixed_text = replaceOneInstance(fileOrignalText)

    filePtr = open(sys.argv[1], "w")
    filePtr.write(fixed_text)
    filePtr.close()

commit_message = """
STYLE: Use "typename" for template parameters

For naming template parameters, typename and class are equivalent. ref:14.1.2:
There is no semantic difference between class and typename in a template-parameter.

The primary purpose of this patch set is to make ITK consistent so that an
enforceable style can be implemented.  The ITK Style guide as been updated
to reflect this change [1].

For tempalate parameters the use of "typename" is preferred over "class". Very
early c++ compilers did not have a "typename" keyword, and "class" was
repurposed for declaring template parameters. It was later discovered that this
lead to ambiguity in some valid code constructs, and the "typename" key word
was added. It is sometimes stated [2] that "typename" is marginally more
expressive in its intent and ITK should consistently use "typename" instead of
"class".

[1] https://www.vtk.org/Wiki/ITK/Coding_Style_Guide
[2] https://blogs.msdn.com/b/slippman/archive/2004/08/11/212768.aspx
"""
