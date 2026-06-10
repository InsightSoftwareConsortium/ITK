#!/usr/bin/env python3
# \author Hans J. Johnson
#
# Migrate a source file away from deprecated vnl_math usage, preferring
# replacements that compile under C++17:
#   - vnl_math:: constants, functions, and predicates -> itk::Math:: equivalents
#   - vnl_math:: re-exports of std functions (max/min/cbrt/hypot) -> std::
#   - vnl_math::abs / vnl_math_abs -> itk::Math::Absolute
#   - legacy vnl_math_* global-function spellings -> the same modern targets
#   - "vnl/vnl_math.h" include -> "itkMath.h"
# vnl_huge_val cannot be rewritten textually (the replacement type depends on
# context); occurrences are reported for manual migration to
# std::numeric_limits<T>::infinity() or itk::NumericTraits<T>::max().

import sys
from collections import OrderedDict
from pathlib import Path

replacements = OrderedDict()

# Includes.
replacements['"vnl/vnl_math.h"'] = '"itkMath.h"'
replacements["<vnl/vnl_math.h>"] = "<itkMath.h>"

# Namespace spellings whose itk::Math name differs (must precede the catch-all).
replacements["vnl_math::max"] = "std::max"
replacements["vnl_math::min"] = "std::min"
replacements["vnl_math::cbrt"] = "std::cbrt"
replacements["vnl_math::hypot"] = "std::hypot"
replacements["vnl_math::abs"] = "itk::Math::Absolute"

# Catch-all: every other deprecated vnl_math:: member (constants, the 14
# functions, isnan/isinf/isfinite/isnormal) maps name-identically.
replacements["vnl_math::"] = "itk::Math::"

# Legacy global-function spellings. Longer names precede their prefixes so
# sequential str.replace cannot corrupt them.
replacements["vnl_math_isnan"] = "itk::Math::isnan"
replacements["vnl_math_isinf"] = "itk::Math::isinf"
replacements["vnl_math_isfinite"] = "itk::Math::isfinite"
replacements["vnl_math_isnormal"] = "itk::Math::isnormal"
replacements["vnl_math_max"] = "std::max"
replacements["vnl_math_min"] = "std::min"
replacements["vnl_math_cuberoot"] = "std::cbrt"
replacements["vnl_math_hypot"] = "std::hypot"
replacements["vnl_math_abs"] = "itk::Math::Absolute"
replacements["vnl_math_angle_0_to_2pi"] = "itk::Math::angle_0_to_2pi"
replacements["vnl_math_angle_minuspi_to_pi"] = "itk::Math::angle_minuspi_to_pi"
replacements["vnl_math_rnd_halfinttoeven"] = "itk::Math::rnd_halfinttoeven"
replacements["vnl_math_rnd_halfintup"] = "itk::Math::rnd_halfintup"
replacements["vnl_math_rnd"] = "itk::Math::rnd"
replacements["vnl_math_floor"] = "itk::Math::floor"
replacements["vnl_math_ceil"] = "itk::Math::ceil"
replacements["vnl_math_sqr"] = "itk::Math::sqr"
replacements["vnl_math_cube"] = "itk::Math::cube"
replacements["vnl_math_sgn0"] = "itk::Math::sgn0"
replacements["vnl_math_sgn"] = "itk::Math::sgn"
replacements["vnl_math_squared_magnitude"] = "itk::Math::squared_magnitude"
replacements["vnl_math_remainder_truncated"] = "itk::Math::remainder_truncated"
replacements["vnl_math_remainder_floored"] = "itk::Math::remainder_floored"


def main() -> int:
    if len(sys.argv) != 2:
        print(f"usage: {Path(sys.argv[0]).name} <source-file>", file=sys.stderr)
        return 2
    cfile = Path(sys.argv[1])
    original_string = cfile.read_text()
    file_as_string = original_string

    for searchval, replaceval in replacements.items():
        file_as_string = file_as_string.replace(searchval, replaceval)

    if "vnl_huge_val" in file_as_string:
        print(
            f"MANUAL FIX NEEDED: {cfile}: vnl_huge_val -> "
            "std::numeric_limits<T>::infinity() or itk::NumericTraits<T>::max()"
        )

    if file_as_string != original_string:
        print(f"Processing: {cfile}")
        cfile.write_text(file_as_string)
    else:
        print(f"SKIPPING: {cfile}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
