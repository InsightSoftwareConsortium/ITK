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

# Usage: pyi_generator_merge_test.py <path to pyi_generator.py>

import importlib.util
import sys

spec = importlib.util.spec_from_file_location("pyi_generator", sys.argv[1])
pyi_generator = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pyi_generator)


def make_class(parents):
    itk_class = pyi_generator.ITKClass("ImageToImageFilter")
    itk_class.parent_class = list(parents)
    return itk_class


# Bases deliberately out of alphabetical order, so neither set nor sorted order passes.
first = make_class(["_ProcessObjectProxy", "_ImageSourceProxy"])
second = make_class(["_ImageSourceProxy", "_LightObjectProxy"])
merged = pyi_generator.merge([first, second])

expected = ["_ProcessObjectProxy", "_ImageSourceProxy", "_LightObjectProxy"]
if merged.parent_class != expected:
    print(f"merged parent_class {merged.parent_class}, expected {expected}")
    sys.exit(1)
