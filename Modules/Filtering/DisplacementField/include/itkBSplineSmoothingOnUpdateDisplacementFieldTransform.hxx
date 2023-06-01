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
#ifndef itkBSplineSmoothingOnUpdateDisplacementFieldTransform_hxx
#define itkBSplineSmoothingOnUpdateDisplacementFieldTransform_hxx


#include "itkImageAlgorithm.h"
#include "itkContinuousIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImportImageFilter.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType,
                                                   VDimension>::BSplineSmoothingOnUpdateDisplacementFieldTransform()

{
  this->m_NumberOfControlPointsForTheUpdateField.Fill(4);
  this->m_NumberOfControlPointsForTheTotalField.Fill(0);
}

template <typename TParametersValueType, unsigned int VDimension>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::SetMeshSizeForTheUpdateField(
  const ArrayType & meshSize)
{
  ArrayType numberOfControlPoints;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
  }
  this->SetNumberOfControlPointsForTheUpdateField(numberOfControlPoints);
}

template <typename TParametersValueType, unsigned int VDimension>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::SetMeshSizeForTheTotalField(
  const ArrayType & meshSize)
{
  ArrayType numberOfControlPoints;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
  }
  this->SetNumberOfControlPointsForTheTotalField(numberOfControlPoints);
}

template <typename TParametersValueType, unsigned int VDimension>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::UpdateTransformParameters(
  const DerivativeType & update,
  ScalarType             factor)
{
  DisplacementFieldPointer displacementField = this->GetModifiableDisplacementField();

  const typename DisplacementFieldType::RegionType & bufferedRegion = displacementField->GetBufferedRegion();
  const SizeValueType                                numberOfPixels = bufferedRegion.GetNumberOfPixels();

  using ImporterType = ImportImageFilter<DisplacementVectorType, Dimension>;
  const bool importFilterWillReleaseMemory = false;

  //
  // Smooth the update field
  //
  bool smoothUpdateField = true;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    if (this->m_NumberOfControlPointsForTheUpdateField[d] <= this->m_SplineOrder)
    {
      itkDebugMacro("Not smooothing the update field.");
      smoothUpdateField = false;
      break;
    }
  }
  if (smoothUpdateField)
  {
    itkDebugMacro("Smooothing the update field.");

    auto * updateFieldPointer =
      reinterpret_cast<DisplacementVectorType *>(const_cast<DerivativeType &>(update).data_block());

    auto importer = ImporterType::New();
    importer->SetImportPointer(updateFieldPointer, numberOfPixels, importFilterWillReleaseMemory);
    importer->SetRegion(displacementField->GetBufferedRegion());
    importer->SetOrigin(displacementField->GetOrigin());
    importer->SetSpacing(displacementField->GetSpacing());
    importer->SetDirection(displacementField->GetDirection());

    DisplacementFieldPointer updateField = importer->GetOutput();
    updateField->Update();
    updateField->DisconnectPipeline();

    DisplacementFieldPointer updateSmoothField =
      this->BSplineSmoothDisplacementField(updateField, this->m_NumberOfControlPointsForTheUpdateField);

    auto * updatePointer = reinterpret_cast<DerivativeValueType *>(updateSmoothField->GetBufferPointer());

    // Add the update field to the current total field
    bool letArrayManageMemory = false;
    // Pass data pointer to required container. No copying is done.
    DerivativeType smoothedUpdate(updatePointer, update.GetSize(), letArrayManageMemory);
    Superclass::UpdateTransformParameters(smoothedUpdate, factor);
  }
  else
  {
    // Add the update field to the current total field
    Superclass::UpdateTransformParameters(update, factor);
  }

  //
  // Smooth the total field
  //
  bool smoothTotalField = true;
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    if (this->m_NumberOfControlPointsForTheTotalField[d] <= this->m_SplineOrder)
    {
      itkDebugMacro("Not smooothing the total field.");
      smoothTotalField = false;
      break;
    }
  }
  if (smoothTotalField)
  {
    itkDebugMacro("Smooothing the total field.");

    auto importer = ImporterType::New();
    importer->SetImportPointer(displacementField->GetBufferPointer(), numberOfPixels, importFilterWillReleaseMemory);
    importer->SetRegion(displacementField->GetBufferedRegion());
    importer->SetOrigin(displacementField->GetOrigin());
    importer->SetSpacing(displacementField->GetSpacing());
    importer->SetDirection(displacementField->GetDirection());

    DisplacementFieldPointer totalField = importer->GetOutput();
    totalField->Update();
    totalField->DisconnectPipeline();

    DisplacementFieldPointer totalSmoothField =
      this->BSplineSmoothDisplacementField(totalField, this->m_NumberOfControlPointsForTheTotalField);

    ImageAlgorithm::Copy<DisplacementFieldType, DisplacementFieldType>(
      totalSmoothField, totalField, totalSmoothField->GetBufferedRegion(), totalField->GetBufferedRegion());
  }
}

template <typename TParametersValueType, unsigned int VDimension>
auto
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::BSplineSmoothDisplacementField(
  const DisplacementFieldType * field,
  const ArrayType &             numberOfControlPoints) -> DisplacementFieldPointer
{
  auto bspliner = BSplineFilterType::New();
  bspliner->SetUseInputFieldToDefineTheBSplineDomain(true);
  bspliner->SetDisplacementField(field);
  bspliner->SetNumberOfControlPoints(numberOfControlPoints);
  bspliner->SetSplineOrder(this->m_SplineOrder);
  bspliner->SetNumberOfFittingLevels(1);
  bspliner->SetEnforceStationaryBoundary(this->m_EnforceStationaryBoundary);
  bspliner->SetEstimateInverse(false);
  bspliner->Update();

  DisplacementFieldPointer smoothField = bspliner->GetOutput();

  return smoothField;
}

template <typename TParametersValueType, unsigned int VDimension>
typename LightObject::Pointer
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::InternalClone() const
{
  LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro("downcast to type " << this->GetNameOfClass() << " failed.");
  }

  //
  // set fields not in the fixed parameters.
  rval->SetSplineOrder(this->GetSplineOrder());

  rval->SetNumberOfControlPointsForTheUpdateField(this->GetNumberOfControlPointsForTheUpdateField());

  rval->SetNumberOfControlPointsForTheTotalField(this->GetNumberOfControlPointsForTheTotalField());

  rval->SetFixedParameters(this->GetFixedParameters());
  rval->SetParameters(this->GetParameters());

  return loPtr;
}

template <typename TParametersValueType, unsigned int VDimension>
void
BSplineSmoothingOnUpdateDisplacementFieldTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os,
                                                                                                Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "SplineOrder: " << static_cast<typename NumericTraits<SplineOrderType>::PrintType>(m_SplineOrder)
     << std::endl;
  os << indent << "EnforceStationaryBoundary: " << (m_EnforceStationaryBoundary ? "On" : "Off") << std::endl;
  os << indent << "NumberOfControlPointsForTheUpdateField: " << m_NumberOfControlPointsForTheUpdateField << std::endl;
  os << indent << "NumberOfControlPointsForTheTotalField: " << m_NumberOfControlPointsForTheTotalField << std::endl;
}
} // namespace itk

#endif
