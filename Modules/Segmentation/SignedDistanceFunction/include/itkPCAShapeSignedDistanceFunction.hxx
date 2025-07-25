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
#ifndef itkPCAShapeSignedDistanceFunction_hxx
#define itkPCAShapeSignedDistanceFunction_hxx

#include "itkTranslationTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNearestNeighborExtrapolateImageFunction.h"

namespace itk
{
// Constructor with default arguments
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::PCAShapeSignedDistanceFunction()
  : m_MeanImage(nullptr)
  , m_Transform(TranslationTransform<TCoordinate, SpaceDimension>::New())
{
  m_PrincipalComponentImages.resize(0);
  m_PrincipalComponentStandardDeviations.SetSize(0);
  m_Interpolators.resize(0);
  m_Extrapolators.resize(0);

  m_WeightOfPrincipalComponents.SetSize(0);
  m_TransformParameters.SetSize(0);
  this->GetParameters().SetSize(0);
}

// Set the number of principal components
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
void
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::SetNumberOfPrincipalComponents(unsigned int n)
{
  m_NumberOfPrincipalComponents = n;

  m_PrincipalComponentImages.resize(n, nullptr);
  m_PrincipalComponentStandardDeviations.SetSize(n);
  m_PrincipalComponentStandardDeviations.Fill(1.0);

  m_WeightOfPrincipalComponents.SetSize(n);
}

// Set the parameters
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
void
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::SetParameters(const ParametersType & parameters)
{
  this->m_Parameters = parameters;

  // set the shape parameters
  for (unsigned int i = 0; i < m_NumberOfPrincipalComponents; ++i)
  {
    m_WeightOfPrincipalComponents[i] = parameters[i];
  }

  // set the transform parameters
  m_NumberOfTransformParameters = parameters.size() - m_NumberOfPrincipalComponents;
  m_TransformParameters.SetSize(m_NumberOfTransformParameters);

  for (unsigned int i = 0; i < m_NumberOfTransformParameters; ++i)
  {
    m_TransformParameters[i] = parameters[m_NumberOfPrincipalComponents + i];
  }

  // set up the transform
  m_Transform->SetParameters(m_TransformParameters);
}

// Print self
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
void
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;

  os << indent << "NumberOfPrincipalComponents: " << m_NumberOfPrincipalComponents << std::endl;
  os << indent << "PrincipalComponentStandardDeviations: " << m_PrincipalComponentStandardDeviations << std::endl;
  os << indent << "MeanImage: " << m_MeanImage.GetPointer() << std::endl;

  os << indent << "WeightOfPrincipalComponents: " << m_WeightOfPrincipalComponents << std::endl;
  os << indent << "TransformParameters: " << m_TransformParameters << std::endl;
}

// Initialize the function
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
void
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::Initialize()
{
  // verify mean image
  if (!m_MeanImage)
  {
    itkExceptionMacro("MeanImage is not present.");
  }

  // verify principal component images
  if (m_PrincipalComponentImages.size() < m_NumberOfPrincipalComponents)
  {
    itkExceptionMacro("PrincipalComponentsImages does not have at least " << m_NumberOfPrincipalComponents
                                                                          << " number of elements.");
  }

  // verify image buffered region
  const typename ImageType::RegionType meanImageRegion = m_MeanImage->GetBufferedRegion();

  for (unsigned int i = 0; i < m_NumberOfPrincipalComponents; ++i)
  {
    if (!m_PrincipalComponentImages[i])
    {
      itkExceptionMacro("PrincipalComponentImages[" << i << "] is not present.");
    }

    if (m_PrincipalComponentImages[i]->GetBufferedRegion() != meanImageRegion)
    {
      itkExceptionMacro("The buffered region of the PrincipalComponentImages[" << i
                                                                               << "] is different from the MeanImage.");
    }
  }

  // set up the interpolators/extrapolators for each of the mean and pc images
  m_Interpolators.resize(m_NumberOfPrincipalComponents + 1);
  m_Extrapolators.resize(m_NumberOfPrincipalComponents + 1);

  // interpolator/extrapolator for mean image
  m_Interpolators[0] = LinearInterpolateImageFunction<ImageType, CoordinateType>::New();
  m_Interpolators[0]->SetInputImage(m_MeanImage);

  m_Extrapolators[0] = NearestNeighborExtrapolateImageFunction<ImageType, CoordinateType>::New();
  m_Extrapolators[0]->SetInputImage(m_MeanImage);

  // interpolators/extrapolators for pc images
  for (unsigned int k = 1; k <= m_NumberOfPrincipalComponents; ++k)
  {
    m_Interpolators[k] = LinearInterpolateImageFunction<ImageType, CoordinateType>::New();
    m_Interpolators[k]->SetInputImage(m_PrincipalComponentImages[k - 1]);

    m_Extrapolators[k] = NearestNeighborExtrapolateImageFunction<ImageType, CoordinateType>::New();
    m_Extrapolators[k]->SetInputImage(m_PrincipalComponentImages[k - 1]);
  }
}

// Evaluate the signed distance
template <typename TCoordinate, unsigned int VSpaceDimension, typename TImage>
auto
PCAShapeSignedDistanceFunction<TCoordinate, VSpaceDimension, TImage>::Evaluate(const PointType & point) const
  -> OutputType
{
  // transform the point into the shape model space
  const PointType mappedPoint = m_Transform->TransformPoint(point);

  itkDebugMacro("mappedPoint:" << mappedPoint);

  using RealType = typename NumericTraits<OutputType>::RealType;
  RealType output;

  if (!m_Interpolators[0]->IsInsideBuffer(mappedPoint))
  {
    itkDebugMacro("use extrapolator");
    output = m_Extrapolators[0]->Evaluate(mappedPoint);

    for (unsigned int i = 0; i < m_NumberOfPrincipalComponents; ++i)
    {
      output += m_Extrapolators[i + 1]->Evaluate(mappedPoint) * m_PrincipalComponentStandardDeviations[i] *
                m_WeightOfPrincipalComponents[i];
    }
  }
  else
  {
    itkDebugMacro("use interpolator");
    output = m_Interpolators[0]->Evaluate(mappedPoint);

    for (unsigned int i = 0; i < m_NumberOfPrincipalComponents; ++i)
    {
      output += m_Interpolators[i + 1]->Evaluate(mappedPoint) * m_PrincipalComponentStandardDeviations[i] *
                m_WeightOfPrincipalComponents[i];
    }
  }

  return output;
}

} // end namespace itk

#endif
