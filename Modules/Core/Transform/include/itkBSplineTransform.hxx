/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkBSplineTransform_hxx
#define itkBSplineTransform_hxx

#include "itkBSplineTransform.h"

#include "itkContinuousIndex.h"
#include "itkImageScanlineConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::BSplineTransform()
  : Superclass()
{

  /** Fixed Parameters store the following information:
   *     grid size
   *     grid origin
   *     grid spacing
   *     grid direction
   *  The size of these is equal to the  NInputDimensions
   */
  // For example 3D image has FixedParameters of:
  // [size[0],size[1],size[2],
  // origin[0],origin[1],origin[2],
  // spacing[0],spacing[1],spacing[2],
  // dir[0][0],dir[1][0],dir[2][0],
  // dir[0][1],dir[1][1],dir[2][1],
  // dir[0][2],dir[1][2],dir[2][2]]


  OriginType meshOrigin;
  meshOrigin.Fill(0.0);
  PhysicalDimensionsType meshPhysical;
  meshPhysical.Fill(1.0);

  DirectionType meshDirection;
  meshDirection.SetIdentity();
  MeshSizeType meshSize;
  meshSize.Fill(1);

  this->m_FixedParameters.SetSize(NDimensions * (NDimensions + 3));

  this->SetFixedParametersFromTransformDomainInformation(meshOrigin, meshPhysical, meshDirection, meshSize);


  this->SetCoefficientImageInformationFromFixedParameters();
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
std::string
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetTransformTypeAsString() const
{
  if (VSplineOrder != 3)
  {
    std::ostringstream n;
    n << Superclass::GetTransformTypeAsString() << "_" << VSplineOrder;
    return n.str();
  }
  return Superclass::GetTransformTypeAsString();
}


template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::NumberOfParametersType
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetNumberOfParameters() const
{
  // The number of parameters equals SpaceDimension * number of
  // of pixels in the grid region.
  return SpaceDimension * this->GetNumberOfParametersPerDimension();
}


template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
typename BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::NumberOfParametersType
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetNumberOfParametersPerDimension() const
{
  // The number of parameters per dimension equals the number of
  // of pixels in the grid region.
  NumberOfParametersType numberOfParametersPerDimension = 1;

  for (unsigned int i = 0; i < SpaceDimension; i++)
  {
    numberOfParametersPerDimension *= static_cast<NumberOfParametersType>(this->m_FixedParameters[i]);
  }
  return numberOfParametersPerDimension;
}


template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetTransformDomainOrigin(const OriginType & origin)
{
  if (this->GetTransformDomainOrigin() != origin)
  {
    this->SetFixedParametersFromTransformDomainInformation(origin,
                                                           this->GetTransformDomainPhysicalDimensions(),
                                                           this->GetTransformDomainDirection(),
                                                           this->GetTransformDomainMeshSize());

    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
auto
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetTransformDomainOrigin() const -> OriginType
{
  OriginType origin;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    const ScalarType spacing = this->m_FixedParameters[2 * NDimensions + i];
    origin[i] = spacing * 0.5 * (SplineOrder - 1);
  }

  origin = this->GetTransformDomainDirection() * origin;

  for (unsigned int i = 0; i < NDimensions; i++)
  {
    const ScalarType grid_origin = this->m_FixedParameters[NDimensions + i];
    origin[i] += grid_origin;
  }
  return origin;
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetTransformDomainPhysicalDimensions(
  const PhysicalDimensionsType & dims)
{
  if (this->GetTransformDomainPhysicalDimensions() != dims)
  {
    this->SetFixedParametersFromTransformDomainInformation(
      this->GetTransformDomainOrigin(), dims, this->GetTransformDomainDirection(), this->GetTransformDomainMeshSize());

    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
auto
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetTransformDomainPhysicalDimensions() const
  -> PhysicalDimensionsType
{
  const MeshSizeType     size = this->GetTransformDomainMeshSize();
  PhysicalDimensionsType physicalDim;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    ScalarType spacing = this->m_FixedParameters[2 * NDimensions + i];
    physicalDim[i] = size[i] * spacing;
  }
  return physicalDim;
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetTransformDomainDirection(
  const DirectionType & direction)
{
  if (this->GetTransformDomainDirection() != direction)
  {
    this->SetFixedParametersFromTransformDomainInformation(this->GetTransformDomainOrigin(),
                                                           this->GetTransformDomainPhysicalDimensions(),
                                                           direction,
                                                           this->GetTransformDomainMeshSize());

    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
auto
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetTransformDomainDirection() const -> DirectionType
{
  DirectionType direction;

  for (unsigned int di = 0; di < NDimensions; di++)
  {
    for (unsigned int dj = 0; dj < NDimensions; dj++)
    {
      const FixedParametersValueType v = this->m_FixedParameters[3 * NDimensions + (di * NDimensions + dj)];
      direction[di][dj] = static_cast<typename DirectionType::ValueType>(v);
    }
  }
  return direction;
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetTransformDomainMeshSize(
  const MeshSizeType & meshSize)
{
  if (this->GetTransformDomainMeshSize() != meshSize)
  {
    this->SetFixedParametersFromTransformDomainInformation(this->GetTransformDomainOrigin(),
                                                           this->GetTransformDomainPhysicalDimensions(),
                                                           this->GetTransformDomainDirection(),
                                                           meshSize);

    this->SetCoefficientImageInformationFromFixedParameters();

    this->Modified();
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
auto
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::GetTransformDomainMeshSize() const -> MeshSizeType
{
  MeshSizeType meshSize;

  for (unsigned int i = 0; i < NDimensions; i++)
  {
    using ValueType = typename MeshSizeType::SizeValueType;
    meshSize[i] = static_cast<ValueType>(this->m_FixedParameters[i]) - SplineOrder;
  }
  return meshSize;
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetFixedParametersFromCoefficientImageInformation()
{
  // Fixed Parameters store the following information:
  //  grid size
  //  grid origin
  //  grid spacing
  //  grid direction

  // Set the grid size parameters
  const SizeType & gridSize = this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize();
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>(gridSize[i]);
  }

  const OriginType & origin = this->m_CoefficientImages[0]->GetOrigin();

  for (unsigned int i = 0; i < NDimensions; i++)
  {
    this->m_FixedParameters[NDimensions + i] = static_cast<FixedParametersValueType>(origin[i]);
  }

  // Set the spacing parameters
  const SpacingType & spacing = this->m_CoefficientImages[0]->GetSpacing();
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    this->m_FixedParameters[2 * NDimensions + i] = static_cast<FixedParametersValueType>(spacing[i]);
  }

  // Set the direction parameters
  const DirectionType & direction = this->m_CoefficientImages[0]->GetDirection();
  for (unsigned int di = 0; di < NDimensions; di++)
  {
    for (unsigned int dj = 0; dj < NDimensions; dj++)
    {
      this->m_FixedParameters[3 * NDimensions + (di * NDimensions + dj)] =
        static_cast<FixedParametersValueType>(direction[di][dj]);
    }
  }
}


template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetFixedParametersFromTransformDomainInformation(
  const OriginType &             meshOrigin,
  const PhysicalDimensionsType & meshPhysical,
  const DirectionType &          meshDirection,
  const MeshSizeType &           meshSize)
{

  // Set the grid size parameters
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>(meshSize[i] + SplineOrder);
  }


  // Set the origin parameters
  using PointType = typename ImageType::PointType;
  PointType origin;
  origin.Fill(0.0);
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    ScalarType gridSpacing = meshPhysical[i] / static_cast<ScalarType>(meshSize[i]);
    origin[i] = -0.5 * gridSpacing * (SplineOrder - 1);
  }

  origin = meshDirection * origin;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    this->m_FixedParameters[NDimensions + i] = static_cast<FixedParametersValueType>(origin[i] + meshOrigin[i]);
  }

  // Set the spacing parameters
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    ScalarType gridSpacing = meshPhysical[i] / static_cast<ScalarType>(meshSize[i]);

    this->m_FixedParameters[2 * NDimensions + i] = static_cast<FixedParametersValueType>(gridSpacing);
  }

  // Set the direction parameters
  for (unsigned int di = 0; di < NDimensions; di++)
  {
    for (unsigned int dj = 0; dj < NDimensions; dj++)
    {
      this->m_FixedParameters[3 * NDimensions + (di * NDimensions + dj)] =
        static_cast<FixedParametersValueType>(meshDirection[di][dj]);
    }
  }
}


template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetCoefficientImageInformationFromFixedParameters()
{
  // Fixed Parameters store the following information:
  //  grid size
  //  grid origin
  //  grid spacing
  //  grid direction
  //  The size of these is equal to the  NInputDimensions

  // set the grid size parameters
  SizeType gridSize;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    gridSize[i] = static_cast<SizeValueType>(this->m_FixedParameters[i]);
  }
  this->m_CoefficientImages[0]->SetRegions(gridSize);

  // Set the origin parameters
  OriginType origin;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    origin[i] = this->m_FixedParameters[NDimensions + i];
  }
  this->m_CoefficientImages[0]->SetOrigin(origin);

  // Set the spacing parameters
  SpacingType spacing;
  for (unsigned int i = 0; i < NDimensions; i++)
  {
    spacing[i] = this->m_FixedParameters[2 * NDimensions + i];
  }
  this->m_CoefficientImages[0]->SetSpacing(spacing);

  // Set the direction parameters
  DirectionType direction;
  for (unsigned int di = 0; di < NDimensions; di++)
  {
    for (unsigned int dj = 0; dj < NDimensions; dj++)
    {
      direction[di][dj] = this->m_FixedParameters[3 * NDimensions + (di * NDimensions + dj)];
    }
  }
  this->m_CoefficientImages[0]->SetDirection(direction);

  // Copy the information to the rest of the images
  for (unsigned int i = 1; i < SpaceDimension; i++)
  {
    this->m_CoefficientImages[i]->CopyInformation(this->m_CoefficientImages[0]);
    this->m_CoefficientImages[i]->SetRegions(this->m_CoefficientImages[0]->GetLargestPossibleRegion());
  }

  // Check if we need to resize the default parameter buffer.
  if (this->m_InternalParametersBuffer.GetSize() != this->GetNumberOfParameters())
  {
    this->m_InternalParametersBuffer.SetSize(this->GetNumberOfParameters());
    // Fill with zeros for identity.
    this->m_InternalParametersBuffer.Fill(0);

    // Set the image's pixel container to this buffer
    this->SetParameters(this->m_InternalParametersBuffer);
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetFixedParameters(
  const FixedParametersType & passedParameters)
{
  // Check if the number of passedParameters match the
  // expected number of this->m_FixedParameters
  if (passedParameters.Size() == this->m_FixedParameters.Size())
  {
    for (unsigned int i = 0; i < NDimensions * (3 + NDimensions); ++i)
    {
      this->m_FixedParameters[i] = passedParameters[i];
    }
    this->Modified();
  }
  else
  {
    itkExceptionMacro(<< "Mismatched between parameters size " << passedParameters.size()
                      << " and the required number of fixed parameters " << this->m_FixedParameters.Size());
  }


  this->SetCoefficientImageInformationFromFixedParameters();
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::SetCoefficientImages(
  const CoefficientImageArray & images)
{
  bool validArrayOfImages = true;

  for (unsigned int j = 0; j < SpaceDimension; j++)
  {
    validArrayOfImages &= (images[j].IsNotNull());
  }

  if (!validArrayOfImages)
  {
    itkExceptionMacro(<< "SetCoefficientImage() requires that an array of "
                      << "correctly sized images be supplied.");
  }


  // update coefficient images and internal parameter buffer
  const SizeValueType numberOfPixels = images[0]->GetLargestPossibleRegion().GetNumberOfPixels();

  const SizeValueType totalParameters = numberOfPixels * SpaceDimension;
  this->m_InternalParametersBuffer.SetSize(totalParameters);
  for (unsigned int j = 0; j < SpaceDimension; j++)
  {
    const SizeValueType numberOfPixels_j = images[j]->GetLargestPossibleRegion().GetNumberOfPixels();
    if (numberOfPixels_j * SpaceDimension != totalParameters)
    {
      itkExceptionMacro(<< "SetCoefficientImage() has array of images that are "
                        << "not the correct size. " << numberOfPixels_j * SpaceDimension << " != " << totalParameters
                        << " for image at index " << j << "  \n"
                        << images[j]);
    }
    const ParametersValueType * const baseImagePointer = images[j]->GetBufferPointer();

    ParametersValueType * dataPointer = this->m_InternalParametersBuffer.data_block();
    std::copy_n(baseImagePointer, numberOfPixels, dataPointer + j * numberOfPixels);

    this->m_CoefficientImages[j]->CopyInformation(images[j]);
    this->m_CoefficientImages[j]->SetRegions(images[j]->GetLargestPossibleRegion());
  }

  // synchronize parameters
  this->SetFixedParametersFromCoefficientImageInformation();
  this->SetParameters(this->m_InternalParametersBuffer);
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
bool
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::InsideValidRegion(ContinuousIndexType & index) const
{
  const SizeType gridSize = this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize();

  const ScalarType minLimit = 0.5 * static_cast<ScalarType>(SplineOrder - 1);

  // Needed so that index can be changed
  bool inside = true;
  for (unsigned int j = 0; j < SpaceDimension; j++)
  {
    const ScalarType maxLimit =
      static_cast<ScalarType>(gridSize[j]) - 0.5 * static_cast<ScalarType>(SplineOrder - 1) - 1.0;
    if (Math::FloatAlmostEqual(index[j], maxLimit, 4))
    {
      index[j] = Math::FloatAddULP(maxLimit, -6);
    }
    else if (index[j] >= maxLimit)
    {
      inside = false;
      break;
    }
    else if (index[j] < minLimit)
    {
      inside = false;
      break;
    }
  }
  return inside;
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::TransformPoint(const InputPointType &    point,
                                                                                  OutputPointType &         outputPoint,
                                                                                  WeightsType &             weights,
                                                                                  ParameterIndexArrayType & indices,
                                                                                  bool & inside) const
{
  inside = true;

  if (this->m_CoefficientImages[0]->GetBufferPointer())
  {
    ContinuousIndexType index;
    this->m_CoefficientImages[0]->TransformPhysicalPointToContinuousIndex(point, index);

    // NOTE: if the support region does not lie totally within the grid
    // we assume zero displacement and return the input point
    inside = this->InsideValidRegion(index);
    if (!inside)
    {
      outputPoint = point;
      return;
    }

    IndexType supportIndex;
    // Compute interpolation weights
    this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

    // For each dimension, correlate coefficient with weights
    SizeType supportSize;
    supportSize.Fill(SplineOrder + 1);
    RegionType supportRegion;
    supportRegion.SetSize(supportSize);
    supportRegion.SetIndex(supportIndex);

    outputPoint.Fill(NumericTraits<ScalarType>::ZeroValue());

    using IteratorType = ImageScanlineConstIterator<ImageType>;
    IteratorType                coeffIterator[SpaceDimension];
    unsigned long               counter = 0;
    const ParametersValueType * basePointer = this->m_CoefficientImages[0]->GetBufferPointer();
    for (unsigned int j = 0; j < SpaceDimension; j++)
    {
      coeffIterator[j] = IteratorType(this->m_CoefficientImages[j], supportRegion);
    }

    while (!coeffIterator[0].IsAtEnd())
    {
      while (!coeffIterator[0].IsAtEndOfLine())
      {
        // Multiply weigth with coefficient
        for (unsigned int j = 0; j < SpaceDimension; j++)
        {
          outputPoint[j] += static_cast<ScalarType>(weights[counter] * coeffIterator[j].Get());
        }

        // Populate the indices array
        indices[counter] = &(coeffIterator[0].Value()) - basePointer;

        // Go to next coefficient in the support region
        ++counter;
        for (unsigned int j = 0; j < SpaceDimension; j++)
        {
          ++(coeffIterator[j]);
        }
      } // end scanline

      for (unsigned int j = 0; j < SpaceDimension; j++)
      {
        coeffIterator[j].NextLine();
      }
    }

    // Return results
    for (unsigned int j = 0; j < SpaceDimension; j++)
    {
      outputPoint[j] += point[j];
    }
  }
  else
  {
    itkWarningMacro("B-spline coefficients have not been set");
    for (unsigned int j = 0; j < SpaceDimension; j++)
    {
      outputPoint[j] = point[j];
    }
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::ComputeJacobianWithRespectToParameters(
  const InputPointType & point,
  JacobianType &         jacobian) const
{
  // Zero all components of jacobian
  jacobian.SetSize(SpaceDimension, this->GetNumberOfParameters());
  jacobian.Fill(0.0);
  RegionType supportRegion;
  SizeType   supportSize;
  supportSize.Fill(SplineOrder + 1);
  supportRegion.SetSize(supportSize);

  ContinuousIndexType index;
  this->m_CoefficientImages[0]->TransformPhysicalPointToContinuousIndex(point, index);

  // NOTE: if the support region does not lie totally within the grid we assume
  // zero displacement and do no computations beyond zeroing out the value
  // return the input point
  if (!this->InsideValidRegion(index))
  {
    return;
  }

  // Compute interpolation weights
  WeightsType weights(this->m_WeightsFunction->GetNumberOfWeights());

  IndexType supportIndex;
  this->m_WeightsFunction->Evaluate(index, weights, supportIndex);

  supportRegion.SetIndex(supportIndex);

  IndexType startIndex = this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetIndex();

  const MeshSizeType meshSize = this->GetTransformDomainMeshSize();
  SizeType           cumulativeGridSizes;
  cumulativeGridSizes[0] = (meshSize[0] + SplineOrder);
  for (unsigned int d = 1; d < SpaceDimension; d++)
  {
    cumulativeGridSizes[d] = cumulativeGridSizes[d - 1] * (meshSize[d] + SplineOrder);
  }

  SizeValueType numberOfParametersPerDimension = this->GetNumberOfParametersPerDimension();

  ImageRegionConstIteratorWithIndex<ImageType> It(this->m_CoefficientImages[0], supportRegion);
  unsigned long                                counter = 0;
  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    typename ImageType::OffsetType currentIndex = It.GetIndex() - startIndex;

    unsigned long number = currentIndex[0];
    for (unsigned int d = 1; d < SpaceDimension; d++)
    {
      number += (currentIndex[d] * cumulativeGridSizes[d - 1]);
    }

    for (unsigned int d = 0; d < SpaceDimension; d++)
    {
      jacobian(d, number + d * numberOfParametersPerDimension) = weights[counter];
    }
    counter++;
  }
}

template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "TransformDomainOrigin: " << this->GetTransformDomainOrigin() << std::endl;
  os << indent << "TransformDomainPhysicalDimensions: " << this->GetTransformDomainPhysicalDimensions() << std::endl;
  os << indent << "TransformDomainDirection: " << this->GetTransformDomainDirection() << std::endl;
  os << indent << "TransformDomainMeshSize: " << this->GetTransformDomainMeshSize() << std::endl;

  os << indent << "GridSize: " << this->m_CoefficientImages[0]->GetLargestPossibleRegion().GetSize() << std::endl;
  os << indent << "GridOrigin: " << this->m_CoefficientImages[0]->GetOrigin() << std::endl;
  os << indent << "GridSpacing: " << this->m_CoefficientImages[0]->GetSpacing() << std::endl;
  os << indent << "GridDirection: " << this->m_CoefficientImages[0]->GetDirection() << std::endl;
}

} // namespace itk


#endif
