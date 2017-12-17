/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFixedPointInverseDisplacementFieldImageFilter_h
#  define itkFixedPointInverseDisplacementFieldImageFilter_h


#  include "itkImageToImageFilter.h"

#  include "itkWarpVectorImageFilter.h"
#  include "itkVectorLinearInterpolateImageFunction.h"
#  include "itkImageRegionIterator.h"
#  include "itkTimeProbe.h"

namespace itk
{

/** \class FixedPointInverseDisplacementFieldImageFilter
 * \brief Computes the inverse of a Displacement field using a fixed point iteration scheme.
 *
 * FixedPointInverseDisplacementFieldImageFilter takes a Displacement field as input and
 * computes the Displacement field that is its inverse. If the input Displacement
 * field was mapping coordinates from a space A into a space B, the output of
 * this filter will map coordinates from the space B into the space A.
 *
 * To compute the inverse of the given Displacement field, the fixed point algorithm by
 * Mingli Chen, Weiguo Lu, Quan Chen, Knneth J. Ruchala and Gusavo H. Olivera
 * described in the paper
 * "A simple fixed-point approach to invert a Displacement field",
 * Medical Physics, vol. 35, issue 1, p. 81,
 * is applied.
 *
