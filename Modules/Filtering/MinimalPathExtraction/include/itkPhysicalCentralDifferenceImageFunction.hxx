/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPhysicalCentralDifferenceImageFunction_hxx
#define itkPhysicalCentralDifferenceImageFunction_hxx


namespace itk
{


template <class TInputImage, class TCoordRep>
PhysicalCentralDifferenceImageFunction<TInputImage, TCoordRep>::PhysicalCentralDifferenceImageFunction()
{
  m_Interpolator = InterpolateImageFunctionType::New();
}


template <class TInputImage, class TCoordRep>
void
PhysicalCentralDifferenceImageFunction<TInputImage, TCoordRep>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}


template <class TInputImage, class TCoordRep>
typename PhysicalCentralDifferenceImageFunction<TInputImage, TCoordRep>::OutputType
PhysicalCentralDifferenceImageFunction<TInputImage, TCoordRep>::Evaluate(const PointType & point) const
{
  OutputType derivative;
  derivative.Fill(0.0);

  for (unsigned int dim = 0; dim < TInputImage::ImageDimension; dim++)
  {
    // Get the left neighbor
    PointType pointLeft(point);
    pointLeft[dim] += -1 * Superclass::m_Image->GetSpacing()[dim];
    TCoordRep valueLeft = m_Interpolator->Evaluate(pointLeft);

    // Get the right neighbor
    PointType pointRight(point);
    pointRight[dim] += 1 * Superclass::m_Image->GetSpacing()[dim];
    TCoordRep valueRight = m_Interpolator->Evaluate(pointRight);

    // Compute derivative
    derivative[dim] = (valueRight - valueLeft) * (0.5 / Superclass::m_Image->GetSpacing()[dim]);
  }

  return (derivative);
}

} // end namespace itk

#endif
