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
import sys
import numpy as np
import itk
import resource


def peak_rss_bytes(who=resource.RUSAGE_SELF) -> int:
    # ru_maxrss is in bytes on macOS and in kilobytes on Linux.
    rss = resource.getrusage(who).ru_maxrss
    return rss if sys.platform == "darwin" else rss * 1024


def exit_if_leaked(M, leak_number):
    if M[5] - M[4] > 1000 * 1024:
        print(f"Memory leak {leak_number}!")
        sys.exit(1)


ImageType = itk.Image[itk.F, 3]
converter = itk.PyBuffer[ImageType]

# adding +1 to numpy created once
inputNumpyVolume = np.ones([100, 100, 100], dtype=np.float32)
n = 10
M = []
X = range(n)
for i in range(n):
    inputNumpyVolume += 1
    inputVolume = converter.GetImageViewFromArray(inputNumpyVolume)
    M.append(peak_rss_bytes())

exit_if_leaked(M, 1)

# creating new numpy volume each time
M = []
X = [x + n for x in range(n)]
for i in range(n):
    inputNumpyVolume = np.ones([100, 100, 100], dtype=np.float32)
    inputVolume = converter.GetImageViewFromArray(inputNumpyVolume)
    M.append(peak_rss_bytes())
exit_if_leaked(M, 2)

# creating new numpy volume but not calling converter.GetImageViewFromArray(inputNumpyVolume)
M = []
X = [x + 2 * n for x in range(n)]
for i in range(n):
    inputNumpyVolume = np.ones([100, 100, 100], dtype=np.float32)
    M.append(peak_rss_bytes())
exit_if_leaked(M, 3)
