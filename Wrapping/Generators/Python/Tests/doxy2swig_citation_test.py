# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================

# Usage: doxy2swig_citation_test.py <path to Wrapping/Generators/Doc>

import os
import sys
import tempfile

sys.path.insert(0, sys.argv[1])
from itk_doxy2swig import d2s_dir

REF = '<ref refid="citelist_1CITEREF_{}" kindref="member">[{}]</ref>'
CLASS_XML = (
    '<doxygen><compounddef kind="class" prot="public">'
    "<compoundname>itk::Foo</compoundname><detaileddescription><para>"
    f"See {REF.format('smith2000', 1)}, {REF.format('doe2001', 2)} "
    f"and again {REF.format('smith2000', 1)}."
    "</para></detaileddescription></compounddef></doxygen>"
)
CITELIST_XML = (
    "<doxygen><compounddef><detaileddescription><para><variablelist>"
    '<varlistentry><term><anchor id="citelist_1CITEREF_smith2000"/>[1]</term>'
    "</varlistentry><listitem><para>A. Smith. Title. 2000.</para></listitem>"
    '<varlistentry><term><anchor id="citelist_1CITEREF_doe2001"/>[2]</term>'
    "</varlistentry><listitem><para>B. Doe. Other. 2001.</para></listitem>"
    "</variablelist></para></detaileddescription></compounddef></doxygen>"
)


def convert(xml_dir, citelist):
    """Convert CLASS_XML in xml_dir and return the output path and its words."""
    files = {"classitk_1_1Foo.xml": CLASS_XML}
    if citelist:
        files["citelist.xml"] = CITELIST_XML
    files["itkFoo.conf"] = (
        os.path.join(xml_dir, "classitk_1_1Foo.xml") + "\titk::Foo\titkFoo\n"
    )
    for name, text in files.items():
        with open(os.path.join(xml_dir, name), "w", encoding="utf-8") as f:
            f.write(text)
    out = os.path.join(xml_dir, "itkFoo_doc.i")
    d2s_dir(os.path.join(xml_dir, "itkFoo.conf"), out)
    with open(out, encoding="utf-8") as f:
        return out, " ".join(f.read().split())


with tempfile.TemporaryDirectory() as with_bib, tempfile.TemporaryDirectory() as no_bib:
    out, text = convert(with_bib, citelist=True)
    assert "See smith2000, doe2001 and again smith2000." in text, text
    assert "[1]" not in text, text
    references = (
        "References: smith2000: A. Smith. Title. 2000. doe2001: B. Doe. Other. 2001."
    )
    assert f'{references} ";' in text, text

    # Unchanged output keeps its timestamp, so SWIG does not rerun.
    os.utime(out, (1e9, 1e9))
    convert(with_bib, citelist=True)
    assert os.path.getmtime(out) == 1e9

    _, text = convert(no_bib, citelist=False)
    assert "See smith2000, doe2001 and again smith2000." in text, text
    assert "References:" not in text, text
