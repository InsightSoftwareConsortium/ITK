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
"""OptimizerParameters.SetHelper takes ownership of its argument.

Without a DISOWN typemap, Python and the m_Helper unique_ptr both free the helper,
and the interpreter aborts at shutdown.  A non-zero exit code from this script is
itself part of the regression check.
"""

import itk

parameters = itk.OptimizerParameters[itk.D](3)
helper = itk.OptimizerParametersHelper[itk.D]()

assert helper.thisown, "Python should own a freshly constructed helper"

parameters.SetHelper(helper)

assert not helper.thisown, "SetHelper must transfer ownership away from Python"

print("Test finished.")
