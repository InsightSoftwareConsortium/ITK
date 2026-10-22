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

import gc
import sys

import itk

calls = []


def callback():
    calls.append(1)


command = itk.PyCommand.New()
assert command.GetCommandCallable() is None

command.SetCommandCallable(callback)
reference_count = sys.getrefcount(callback)
for _ in range(10):
    assert command.GetCommandCallable() is callback
gc.collect()
assert sys.getrefcount(callback) == reference_count

command.Execute(itk.Object.New(), itk.AnyEvent())
assert len(calls) == 1
