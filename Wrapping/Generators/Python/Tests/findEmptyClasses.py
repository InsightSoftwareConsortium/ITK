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
import re
import sys
# itk.auto_progress(True)

from itkTemplate import itkTemplate

# sets are not in builtin with python older than 2.4
import sets
set = sets.Set

# dirty but easier: a global var to count the empty classes
empty = set()


def exploreTpl(tpl):
    for cl in tpl.itervalues():
        print(cl)
        exploreMethods(cl)
        # try to instanciate the class
        try:
            obj = cl.New()
            exploreMethods(obj)
        except:
            pass
        try:
            exploreMethods(cl())
        except:
            pass


def exploreMethods(obj):
    global count
    excludeList = ['this', 'thisown']
    attrNameList = [i for i in dir(
        obj) if isinstance(i, str) and i[0].isupper() and i not in excludeList]
    if attrNameList == []:
        empty.add(obj)


excluded = set([
    "PeriodicBoundaryCondition",
    "BandNode",
    "DefaultDynamicMeshTraits",
    "DefaultStaticMeshTraits",
    "NormalBandNode",
    "ZeroFluxNeumannBoundaryCondition",
    "SparseFieldLevelSetNode",
    "ParallelSparseFieldLevelSetNode",
    "PySwigIterator",
    "SwigPyIterator",
    "COLORS",
    "VECTOR_REALS",
    "SCALARS",
    "ALL_TYPES",
    "COMPLEX_REALS",
    "RGBS",
    "RGBAS",
    "REALS",
    "USIGN_INTS",
    "DIMS",
    "SIGN_INTS",
    "VECTORS",
    "INTS",
    "COV_VECTOR_REALS",
    "FFTComplexToComplexImageFilter",
    "QuadEdgeMeshCellTraitsInfo",
    "QuadEdgeMeshTraits",
    "OnesMatrixCoefficients",
    "ConformalMatrixCoefficients",
    "InverseEuclideanDistanceMatrixCoefficients",
    "AuthalicMatrixCoefficients",
    "IntrinsicMatrixCoefficients",
    "InverseEuclideanDistanceMatrixCoefficients",
    "OnesMatrixCoefficients",
    "ConformalMatrixCoefficients",
    "AuthalicMatrixCoefficients",
    "MatrixCoefficients",
])


attrNameList = set(
    [i for i in dir(itk) if i[0].isupper() and len(i) > 2]) - excluded

for name in attrNameList:
    # use it because of lazy loading
    exec "attr = itk." + name
    print("-----------", name, "-----------")
    if isinstance(attr, itkTemplate):
        exploreTpl(attr)
    else:
        exploreMethods(attr)
        try:
            exploreMethods(cl.New())
        except:
            pass
        try:
            exploreMethods(cl())
        except:
            pass

print()
print()
print(len(empty), "empty classes found")
for c in empty:
    print(c)

sys.exit(len(empty))
