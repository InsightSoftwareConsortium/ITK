#==========================================================================
#
#   Copyright NumFOCUS
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

import itk
import sys
from itkTemplate import itkTemplate

itk.auto_progress(2)

itk.force_load()

def isEmpty(o):
    for i in dir(o):
        if i[0].isupper():
            return False
        return True


exclude = ["AuthalicMatrixCoefficients",
           "MatrixCoefficients",
           "OnesMatrixCoefficients",
           "IntrinsicMatrixCoefficients",
           "HarmonicMatrixCoefficients",
           "ConformalMatrixCoefficients",
           "InverseEuclideanDistanceMatrixCoefficients",
           "BandNode",
           "NormalBandNode",
           "CellTraitsInfo",
           "DefaultDynamicMeshTraits",
           "DefaultStaticMeshTraits",
           "ParallelSparseFieldLevelSetNode",
           "SparseFieldLevelSetNode",
           "QuadEdgeMeshCellTraitsInfo",
           "QuadEdgeMeshTraits",
           "complex",
           "list",
           "map",
           "numeric_limits",
           "set",
           "vector",
           "vnl_c_vector",
           "vnl_diag_matrix",
           "vnl_matrix",
           "vnl_matrix_fixed",
           "vnl_matrix_fixed_ref",
           "vnl_matrix_fixed_ref_const",
           "vnl_matrix_ref",
           "vnl_vector",
           "vnl_vector_ref",
           "vnl_file_matrix",
           "vnl_file_vector",
           "vnl_fortran_copy",
           "CosineWindowFunction",
           "HammingWindowFunction",
           "LanczosWindowFunction",
           "WelchWindowFunction",
           ]

total = 0
empty = 0

for t in dir(itk):
    if t not in exclude:
        T = itk.__dict__[t]
        if isinstance(T, itkTemplate):
            for I in T.values():
                total += 1
                if isEmpty(I):
                    empty += 1
                    print("%s: empty class" % I)

print("%s classes checked." % total)
if empty:
    print("%s empty classes found" % empty, file=sys.stderr)
    sys.exit(1)
