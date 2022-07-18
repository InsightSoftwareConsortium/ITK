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
# ==========================================================================*/

from itk.support.build_options import ALL_TYPES, DIMS

# Simple exercise of types identified during build
print("-------- PRINTING THE BUILD DIMENSIONS -----------")
for build_dims in DIMS:
    print(f"ITK Compiled with support for dimension: {build_dims}")
print("---------------------------------------------")
print("-------- PRINTING THE BUILD TYPES -----------")
for build_types_identified in ALL_TYPES:
    print(f"ITK Compiled with support for: {build_types_identified}")
print("---------------------------------------------")
