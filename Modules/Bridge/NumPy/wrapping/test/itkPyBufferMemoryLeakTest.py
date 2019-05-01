#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/
import sys
try:
    import numpy as np
except ImportError:
    # We don't have numpy -- bail
    sys.exit(0)
import itk
import resource

ImageType = itk.Image[itk.F, 3]
converter = itk.PyBuffer[ImageType]

# adding +1 to numpy created once
inputNumpyVolume = np.ones([100,100,100], dtype=np.float32)
n = 10
M = []
X = range(n)
for i in range(n):
    inputNumpyVolume += 1
    inputVolume = converter.GetImageViewFromArray(inputNumpyVolume)
    M.append(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)

if M[5] - M[4] > 1000:
    print('Memory leak!')
    sys.exit(1)

# creating new numpy volume each time
M = []
X = [x + n for x in range(n)]
for i in range(n):
    inputNumpyVolume = np.ones([100,100,100], dtype=np.float32)
    inputVolume = converter.GetImageViewFromArray(inputNumpyVolume)
    M.append(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
if M[5] - M[4] > 1000:
    print('Memory leak!')
    sys.exit(1)

# creating new numpy volume but not calling converter.GetImageViewFromArray(inputNumpyVolume)
M = []
X = [x + 2*n for x in range(n)]
for i in range(n):
    inputNumpyVolume = np.ones([100,100,100], dtype=np.float32)
    M.append(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
if M[5] - M[4] > 1000:
    print('Memory leak!')
    sys.exit(1)
