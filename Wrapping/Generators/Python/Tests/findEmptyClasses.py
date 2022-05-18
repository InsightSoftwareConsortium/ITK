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
# ==========================================================================*/

import itk
import sys
from itk.support.template_class import itkTemplate

itk.auto_progress(2)

itk.force_load()

exclude = [
    "AuthalicMatrixCoefficients",
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
    "PyVectorContainer",
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
    "Tile",  # include from itkTileMontage remote module but only templated over dimension.
    "cvar",
]


def isEmpty(itk_template_class_object):
    for template_member_function in dir(itk_template_class_object):
        if template_member_function[0].isupper():
            # For ITK, if least one capitalized member function is found
            # assume that the class is not empty
            return False
        return True


num_itk_template_classes_checked = 0
num_itk_empty_template_classes = 0

for module_element_name, module_element_object in itk.__dict__.items():
    if module_element_name not in exclude:
        module_element_object = itk.__dict__[module_element_name]
        if isinstance(module_element_object, itkTemplate):
            for template_variant in module_element_object.values():
                num_itk_template_classes_checked += 1
                if isEmpty(template_variant):
                    num_itk_empty_template_classes += 1
                    print(f"{template_variant}: empty class")

print(f"{num_itk_template_classes_checked} classes checked.")
if num_itk_empty_template_classes:
    print(f"{num_itk_empty_template_classes} empty classes found", file=sys.stderr)
    sys.exit(1)
