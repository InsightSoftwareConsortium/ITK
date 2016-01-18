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

from __future__ import print_function

import itk
import sys
import gc

# create some vars used later

idx = itk.Index[2]()
idx.Fill(12)

size = itk.Size[2]()
size.Fill(12)

t = (12, 12)

l = [12, 12]

i = 12

# ok, now lets try all those equivalent vars on some methods
idx1 = itk.Index[2](idx)
idx2 = itk.Index[2](t)
idx3 = itk.Index[2](l)
idx4 = itk.Index[2](i)
assert idx1.GetElement(
    0) == idx2.GetElement(
    0) == idx3.GetElement(
    0) == idx4.GetElement(
    0) == i
assert idx1.GetElement(
    1) == idx2.GetElement(
    1) == idx3.GetElement(
    1) == idx4.GetElement(
    1) == i

size1 = itk.Size[2](size)
size2 = itk.Size[2](t)
size3 = itk.Size[2](l)
size4 = itk.Size[2](i)
assert size1.GetElement(
    0) == size2.GetElement(
    0) == size3.GetElement(
    0) == size4.GetElement(
    0) == i
assert size1.GetElement(
    1) == size2.GetElement(
    1) == size3.GetElement(
    1) == size4.GetElement(
    1) == i

median = itk.MedianImageFilter.IUC2IUC2.New()
median.SetRadius(size)
size1 = median.GetRadius()
median.SetRadius(t)
size2 = median.GetRadius()
median.SetRadius(l)
size3 = median.GetRadius()
median.SetRadius(i)
size4 = median.GetRadius()
assert size1.GetElement(
    0) == size2.GetElement(
    0) == size3.GetElement(
    0) == size4.GetElement(
    0) == i
assert size1.GetElement(
    1) == size2.GetElement(
    1) == size3.GetElement(
    1) == size4.GetElement(
    1) == i


# smart pointers
im = itk.Image.UC2.New()
assert im is not None

median.SetInput(im)
assert median.GetInput() == im
assert median.GetInput() != median.GetOutput()

median.SetInput(None)
assert median.GetInput() is None


# ImageSource

median2 = itk.MedianImageFilter.IUC2IUC2.New()
median.SetInput(median2.GetOutput())
assert median.GetInput() == median2.GetOutput()


# catching exception
try:
    median.Update()
    print("Exception not throwed!", file=sys.stderr)
    sys.exit(1)
except RuntimeError as e:
    print("Exception catched as expected", e)

#   ----- keep that at the end! -----

# pycommand masked
def exit():
    print('exitting on delete...')
    sys.exit(0)
median.AddObserver(itk.DeleteEvent(), exit)
del median

# we shouldn't reach that point
sys.exit(1)
