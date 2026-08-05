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

"""Canary for the two VTK wrapper encodings the ITKVtkGlue abi3 typemaps rely on.

VtkGlue.i exchanges pointers with VTK through `__this__` and the `Addr=0x...`
argument to `__new__` rather than through vtkPythonUtil, because vtkPythonUtil's
header chain is not usable under Py_LIMITED_API. Neither encoding is documented
VTK API, so this test fails loudly and specifically if VTK changes either one.

`__new__` is used rather than plain construction because vtkmodules.util.data_model
registers keyword-only `override` subclasses for vtkImageData and vtkPolyData;
`cls(addr)` reaches those and raises TypeError.

Both encodings and the `__new__` string branch are present in every VTK 9.x from
9.0.0 onward, so the module's VTK 9.1 floor is unchanged. The branch is gated on
the type not being a heap type, which is the thing to re-check if VTK ever makes
its wrapped types heap types.
"""

import re
import sys

from vtkmodules.vtkCommonDataModel import vtkImageData, vtkPolyData
from vtkmodules.vtkIOImage import vtkImageExport, vtkImageImport

# `_<2*sizeof(void*) hex digits>_p_<ClassName>`, per vtkPythonUtil::ManglePointer.
THIS_RE = re.compile(r"^_([0-9a-fA-F]+)_p_(\w+)$")

failures = []


def check(condition, message):
    if not condition:
        failures.append(message)


for cls in (vtkImageData, vtkPolyData, vtkImageExport, vtkImageImport):
    name = cls.__name__
    obj = cls()

    this = getattr(obj, "__this__", None)
    check(this is not None, f"{name}: instance has no __this__ attribute")
    if this is None:
        continue

    match = THIS_RE.match(this)
    check(
        match is not None,
        f"{name}: __this__ {this!r} does not match _<hex>_p_<ClassName>",
    )
    if match is None:
        continue

    address = int(match.group(1), 16)
    check(address != 0, f"{name}: __this__ encodes a null address")
    check(
        match.group(2) == name,
        f"{name}: __this__ encodes class {match.group(2)!r}, expected {name!r}",
    )

    # The reconstruction path VtkGlue.i's `out` typemaps drive.
    try:
        rebuilt = cls.__new__(cls, f"Addr=0x{address:x}")
    except Exception as exception:  # noqa: BLE001 - report any refusal verbatim
        failures.append(f"{name}: Addr=0x... reconstruction raised {exception!r}")
        continue

    rebuilt_this = getattr(rebuilt, "__this__", None)
    check(
        rebuilt_this == this,
        f"{name}: reconstruction yielded __this__ {rebuilt_this!r}, expected {this!r}",
    )
    check(
        isinstance(rebuilt, cls),
        f"{name}: reconstruction yielded {type(rebuilt)!r}, not a {name} instance",
    )

if failures:
    print("VTK wrapper encodings assumed by VtkGlue.i have changed:", file=sys.stderr)
    for failure in failures:
        print(f"  - {failure}", file=sys.stderr)
    sys.exit(1)

print("VTK __this__ and Addr=0x... encodings are intact.")
