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

import time

start = time.time()
import itk
itk.force_load()
print("load:", time.time() - start)

img = itk.Image.UC2.New(Regions=(10, 20))
img.Allocate()


def index(it):
    idx = itk.Index[2]()
    for dummy in range(0, it):
        img.GetPixel(idx)


def index2(it):
    idx = itk.Index[2]()
    for dummy in range(0, it):
        idx[0] = 0
        idx[1] = 0
        img.GetPixel(idx)


def integer(it):
    for dummy in range(0, it):
        img.GetPixel(0)


def pylist(it):
    l = [0, 0]
    for dummy in range(0, it):
        img.GetPixel(l)


def pytuple(it):
    l = (0, 0)
    for dummy in range(0, it):
        img.GetPixel(l)


def new(it):
    for dummy in range(0, it):
        lo = itk.Image.UC2.__New_orig__()


def extended_new(it):
    for dummy in range(0, it):
        lo = itk.Image.UC2.New()


def run(f, it):
    start = time.time()
    f(it)
    return time.time() - start


it = 1000000
print("index:", run(index, it))
print("index2:", run(index2, it))
print("integer:", run(integer, it))
print("list:", run(pylist, it))
print("tuple:", run(pytuple, it))
print("new:", run(new, 100000))
print("extended_new:", run(extended_new, 100000))
