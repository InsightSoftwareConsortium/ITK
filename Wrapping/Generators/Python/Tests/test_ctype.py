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

import itk
import unittest
import numpy as np
import os


class CTypeTestCase(unittest.TestCase):
    """Tests itkCType"""

    def test_dtype_of_ctype_aliases_for_specific_sizes(self) -> None:
        """Tests that each `CType` alias for a specific size has the corresponding `dtype`"""

        # Check uint8 to uint64 and int8 to int64:
        for number_of_bits in (8, 16, 32, 64):
            self.assertEqual(
                getattr(itk, f"int{number_of_bits}_t").dtype,
                getattr(np, f"int{number_of_bits}"),
            )
            self.assertEqual(
                getattr(itk, f"uint{number_of_bits}_t").dtype,
                getattr(np, f"uint{number_of_bits}"),
            )

        # Check float32 and float64:
        for number_of_bits in (32, 64):
            self.assertEqual(
                getattr(itk, f"float{number_of_bits}_t").dtype,
                getattr(np, f"float{number_of_bits}"),
            )

    def test_ctype_aliases_for_64_bit_integers(self) -> None:
        """Tests `int64_t` and `uint64_t`, for both Windows and non-Windows"""

        if os.name == "nt":
            # On Windows, only `long long` integer types (SLL and ULL) are 64-bit.
            self.assertEqual(
                itk.int64_t,
                itk.SLL,
            )
            self.assertEqual(
                itk.uint64_t,
                itk.ULL,
            )
        else:
            # On Linux and MacOS, `long` integer types (SL and UL) are already 64-bit.
            self.assertEqual(
                itk.int64_t,
                itk.SL,
            )
            self.assertEqual(
                itk.uint64_t,
                itk.UL,
            )


if __name__ == "__main__":
    unittest.main()
