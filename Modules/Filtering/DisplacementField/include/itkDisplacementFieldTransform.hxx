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
#ifndef itkDisplacementFieldTransform_hxx
#define itkDisplacementFieldTransform_hxx

#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkImageToImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "itkCastImageFilter.h"

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
DisplacementFieldTransform<TParametersValueType, VDimension>::DisplacementFieldTransform()
  : Superclass(0)
  , m_CoordinateTolerance(ImageToImageFilterCommon::GetGlobalDefaultCoordinateTolerance())
  , m_DirectionTolerance(ImageToImageFilterCommon::GetGlobalDefaultDirectionTolerance())
{
  this->m_FixedParameters.SetSize(VDimension * (VDimension + 3));
  this->m_FixedParameters.Fill(0.0);

  // Setup and assign default interpolator
  using DefaultInterpolatorType = VectorLinearInterpolateImageFunction<DisplacementFieldType, ScalarType>;
  auto interpolator = DefaultInterpolatorType::New();
  this->m_Interpolator = interpolator;

  auto inverseInterpolator = DefaultInterpolatorType::New();
  this->m_InverseInterpolator = inverseInterpolator;

  // Setup and assign parameter helper. This will hold the displacement field
  // for access through the common OptimizerParameters interface.
  auto * helper = new OptimizerParametersHelperType;
  // After assigning this, m_Parameters will manage this,
  // deleting when appropriate.
  this->m_Parameters.SetHelper(helper);

  /* Initialize the identity jacobian. */
  m_IdentityJacobian.SetSize(VDimension, VDimension);
  m_IdentityJacobian.Fill(0.0);
  for (unsigned int dim = 0; dim < VDimension; ++dim)
  {
    m_IdentityJacobian[dim][dim] = 1.0;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
auto
DisplacementFieldTransform<TParametersValueType, VDimension>::TransformPoint(const InputPointType & inputPoint) const
  -> OutputPointType
{
  if (!this->m_DisplacementField)
  {
    itkExceptionMacro("No displacement field is specified.");
  }
  if (!this->m_Interpolator)
  {
    itkExceptionMacro("No interpolator is specified.");
  }

  typename InterpolatorType::PointType point;
  point.CastFrom(inputPoint);

  OutputPointType outputPoint;
  outputPoint.CastFrom(inputPoint);

  if (this->m_Interpolator->IsInsideBuffer(point))
  {
    const typename InterpolatorType::ContinuousIndexType cidx =
      this->m_DisplacementField
        ->template TransformPhysicalPointToContinuousIndex<typename InterpolatorType::ContinuousIndexType::ValueType>(
          point);
    typename InterpolatorType::OutputType displacement = this->m_Interpolator->EvaluateAtContinuousIndex(cidx);
    for (unsigned int ii = 0; ii < VDimension; ++ii)
    {
      outputPoint[ii] += displacement[ii];
    }
  }
  // else
  // simply return inputPoint

  return outputPoint;
}

template <typename TParametersValueType, unsigned int VDimension>
bool
DisplacementFieldTransform<TParametersValueType, VDimension>::GetInverse(Self * inverse) const
{
  if (!inverse || !this->m_InverseDisplacementField)
  {
    return false;
  }
  else
  {
    inverse->SetFixedParameters(this->GetFixedParameters());
    inverse->SetDisplacementField(this->m_InverseDisplacementField);
    inverse->SetInverseDisplacementField(this->m_DisplacementField);
    inverse->SetInterpolator(this->m_InverseInterpolator);
    inverse->SetInverseInterpolator(this->m_Interpolator);

    return true;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
auto
DisplacementFieldTransform<TParametersValueType, VDimension>::GetInverseTransform() const -> InverseTransformBasePointer
{
  Pointer inverseTransform = New();

  if (this->GetInverse(inverseTransform))
  {
    return inverseTransform.GetPointer();
  }
  else
  {
    return nullptr;
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetIdentity()
{
  if (!this->m_DisplacementField.IsNull())
  {
    this->m_DisplacementField->FillBuffer(OutputVectorType());
  }
  if (!this->m_InverseDisplacementField.IsNull())
  {
    this->m_InverseDisplacementField->FillBuffer(OutputVectorType());
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToPosition(
  const InputPointType & point,
  JacobianPositionType & jacobian) const
{
  const auto idx = m_DisplacementField->TransformPhysicalPointToIndex(point);
  this->ComputeJacobianWithRespectToPosition(idx, jacobian);
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToPosition(
  const IndexType &      index,
  JacobianPositionType & jacobian) const
{
  this->ComputeJacobianWithRespectToPositionInternal(index, jacobian, false);
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::ComputeInverseJacobianWithRespectToPosition(
  const InputPointType &        point,
  InverseJacobianPositionType & jacobian) const
{
  const auto idx = m_DisplacementField->TransformPhysicalPointToIndex(point);
  this->ComputeJacobianWithRespectToPositionInternal(idx, jacobian, true);
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::GetInverseJacobianOfForwardFieldWithRespectToPosition(
  const InputPointType & point,
  JacobianPositionType & jacobian,
  bool                   useSVD) const
{
  const auto idx = m_DisplacementField->TransformPhysicalPointToIndex(point);
  this->GetInverseJacobianOfForwardFieldWithRespectToPosition(idx, jacobian, useSVD);
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::GetInverseJacobianOfForwardFieldWithRespectToPosition(
  const IndexType &      index,
  JacobianPositionType & jacobian,
  bool                   useSVD) const
{
  if (useSVD)
  {
    this->ComputeJacobianWithRespectToPositionInternal(index, jacobian, false);
    vnl_svd<typename JacobianPositionType::element_type> svd{ jacobian.as_ref() };
    for (unsigned int i = 0; i < jacobian.rows(); ++i)
    {
      for (unsigned int j = 0; j < jacobian.cols(); ++j)
      {
        jacobian(i, j) = svd.inverse()(i, j);
      }
    }
  }
  else
  {
    this->ComputeJacobianWithRespectToPositionInternal(index, jacobian, true);
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToPositionInternal(
  const IndexType &      index,
  JacobianPositionType & jacobian,
  bool                   doInverseJacobian) const
{

  typename DisplacementFieldType::IndexType startingIndex =
    this->m_DisplacementField->GetLargestPossibleRegion().GetIndex();
  typename DisplacementFieldType::IndexType upperIndex =
    this->m_DisplacementField->GetLargestPossibleRegion().GetUpperIndex();
  typename DisplacementFieldType::SpacingType spacing = this->m_DisplacementField->GetSpacing();

  // Space between indices
  TParametersValueType space = NumericTraits<TParametersValueType>::OneValue();

  // Flag indicating a valid location for Jacobian calculation
  bool isValidJacobianCalcLocat = true;

  // Multiplier for getting inverse Jacobian
  TParametersValueType dPixSign = NumericTraits<TParametersValueType>::OneValue();
  dPixSign = doInverseJacobian ? -dPixSign : dPixSign;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    if (index[i] <= startingIndex[i] || index[i] >= upperIndex[i])
    {
      isValidJacobianCalcLocat = false;
      break;
    }
  }

  if (isValidJacobianCalcLocat)
  {
    // itkCentralDifferenceImageFunction does not support 4th order so
    // do manually here
    for (unsigned int col = 0; col < VDimension; ++col)
    {
      IndexType difIndex[4] = { index, index, index, index };
      difIndex[0][col] -= 2;
      difIndex[1][col] -= 1;
      difIndex[2][col] += 1;
      difIndex[3][col] += 2;
      if (difIndex[0][col] < startingIndex[col])
      {
        difIndex[0][col] = startingIndex[col];
      }
      if (difIndex[3][col] > upperIndex[col])
      {
        difIndex[3][col] = upperIndex[col];
      }

      OutputVectorType pixDisp[4];
      for (unsigned int i = 0; i < 4; ++i)
      {
        pixDisp[i] = m_DisplacementField->GetPixel(difIndex[i]);
      }

      // 4th order centered difference
      OutputVectorType dPix =
        (pixDisp[0] - pixDisp[1] * 8.0 + pixDisp[2] * 8.0 - pixDisp[3]) / (12.0 * space * spacing[col]) * dPixSign;

      for (unsigned int row = 0; row < VDimension; ++row)
      {
        jacobian(row, col) = dPix[row];
        // Verify it's a real number
        if (!itk::Math::isfinite(dPix[row]))
        {
          isValidJacobianCalcLocat = false;
          break;
        }
      }
    } // for col

    for (unsigned int row = 0; row < VDimension; ++row)
    {
      FixedArray<TParametersValueType, VDimension> localComponentGrad(jacobian[row]);
      FixedArray<TParametersValueType, VDimension> physicalComponentGrad =
        m_DisplacementField->TransformLocalVectorToPhysicalVector(localComponentGrad);
      jacobian.set_row(row, physicalComponentGrad.data());
      jacobian(row, row) += 1.;
    }
  } // if isValidJacobianCalcLocat

  if (!isValidJacobianCalcLocat)
  {
    jacobian.set_identity();
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::UpdateTransformParameters(const DerivativeType & update,
                                                                                        ScalarType             factor)
{
  // This simply adds the values.
  // TODO: This should be multi-threaded probably, via image add filter.
  Superclass::UpdateTransformParameters(update, factor);
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetDisplacementField(DisplacementFieldType * field)
{
  if (this->m_DisplacementField != field)
  {
    this->m_DisplacementField = field;

    if (!this->m_InverseDisplacementField.IsNull())
    {
      this->m_InverseDisplacementField = nullptr;
    }
    this->Modified();

    // Store this separately for use in smoothing because we only want
    // to know when the displacement field object has changed, not just
    // its contents.
    this->m_DisplacementFieldSetTime = this->GetMTime();
    if (!this->m_Interpolator.IsNull() && !this->m_DisplacementField.IsNull())
    {
      this->m_Interpolator->SetInputImage(this->m_DisplacementField);
    }
    // Assign to parameters object
    this->m_Parameters.SetParametersObject(this->m_DisplacementField);
  }
  this->SetFixedParametersFromDisplacementField();
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetDisplacementField(
  VectorImageDisplacementFieldType * field)
{
  using CasterType = CastImageFilter<VectorImageDisplacementFieldType, DisplacementFieldType>;
  auto caster = CasterType::New();
  caster->SetInput(field);
  caster->Update();
  this->SetDisplacementField(caster->GetOutput());
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetInverseDisplacementField(
  DisplacementFieldType * inverseField)
{
  if (this->m_InverseDisplacementField != inverseField)
  {
    this->m_InverseDisplacementField = inverseField;
    if (!this->m_DisplacementField.IsNull() && inverseField)
    {
      this->VerifyFixedParametersInformation();
    }
    if (!this->m_InverseInterpolator.IsNull() && !this->m_InverseDisplacementField.IsNull())
    {
      this->m_InverseInterpolator->SetInputImage(this->m_InverseDisplacementField);
    }
    this->Modified();
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::VerifyFixedParametersInformation()
{
  if (!this->m_DisplacementField.IsNull() && !this->m_InverseDisplacementField.IsNull())
  {
    // Check to see if the candidate inverse displacement field has the
    // same fixed parameters as the displacement field.

    SizeType      inverseFieldSize = this->m_InverseDisplacementField->GetLargestPossibleRegion().GetSize();
    PointType     inverseFieldOrigin = this->m_InverseDisplacementField->GetOrigin();
    SpacingType   inverseFieldSpacing = this->m_InverseDisplacementField->GetSpacing();
    DirectionType inverseFieldDirection = this->m_InverseDisplacementField->GetDirection();

    SizeType      fieldSize = this->m_DisplacementField->GetLargestPossibleRegion().GetSize();
    PointType     fieldOrigin = this->m_DisplacementField->GetOrigin();
    SpacingType   fieldSpacing = this->m_DisplacementField->GetSpacing();
    DirectionType fieldDirection = this->m_DisplacementField->GetDirection();

    // Tolerance for origin and spacing depends on the size of pixel
    // tolerance for directions a fraction of the unit cube.
    const double coordinateTolerance = m_CoordinateTolerance * fieldSpacing[0];
    const double directionTolerance = m_DirectionTolerance;

    std::ostringstream sizeString;
    std::ostringstream originString;
    std::ostringstream spacingString;
    std::ostringstream directionString;

    bool unequalSizes = false;
    bool unequalOrigins = false;
    bool unequalSpacings = false;
    bool unequalDirections = false;

    if (inverseFieldSize != fieldSize)
    {
      unequalSizes = true;
      sizeString << "InverseDisplacementField Size: " << inverseFieldSize << ", DisplacementField Size: " << fieldSize
                 << std::endl;
    }
    if (!inverseFieldOrigin.GetVnlVector().is_equal(fieldOrigin.GetVnlVector(), coordinateTolerance))
    {
      unequalOrigins = true;
      originString << "InverseDisplacementField Origin: " << inverseFieldOrigin
                   << ", DisplacementField Origin: " << fieldOrigin << std::endl;
    }
    if (!inverseFieldSpacing.GetVnlVector().is_equal(fieldSpacing.GetVnlVector(), coordinateTolerance))
    {
      unequalSpacings = false;
      originString << "InverseDisplacementField Spacing: " << inverseFieldSpacing
                   << ", DisplacementField Spacing: " << fieldSpacing << std::endl;
    }
    if (!inverseFieldDirection.GetVnlMatrix().as_ref().is_equal(fieldDirection.GetVnlMatrix().as_ref(),
                                                                directionTolerance))
    {
      unequalDirections = true;
      originString << "InverseDisplacementField Direction: " << inverseFieldDirection
                   << ", DisplacementField Direction: " << fieldDirection << std::endl;
    }
    if (unequalSizes || unequalOrigins || unequalSpacings || unequalDirections)
    {
      itkExceptionMacro("The inverse and displacement fields do not have the same fixed parameters: "
                        << std::endl
                        << sizeString.str() << originString.str() << spacingString.str() << directionString.str());
    }
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetInterpolator(InterpolatorType * interpolator)
{
  if (this->m_Interpolator != interpolator)
  {
    this->m_Interpolator = interpolator;
    this->Modified();
    if (!this->m_DisplacementField.IsNull() && !this->m_Interpolator.IsNull())
    {
      this->m_Interpolator->SetInputImage(this->m_DisplacementField);
    }
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetInverseInterpolator(InterpolatorType * interpolator)
{
  if (this->m_InverseInterpolator != interpolator)
  {
    this->m_InverseInterpolator = interpolator;
    this->Modified();
    if (!this->m_InverseDisplacementField.IsNull() && !this->m_InverseInterpolator.IsNull())
    {
      this->m_InverseInterpolator->SetInputImage(this->m_InverseDisplacementField);
    }
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetFixedParameters(
  const FixedParametersType & fixedParameters)
{
  if (fixedParameters.Size() != VDimension * (VDimension + 3))
  {
    itkExceptionMacro("The fixed parameters are not the right size.");
  }

  bool nullState = true;
  for (unsigned int i = 0; i < fixedParameters.Size() && nullState; ++i)
  {
    nullState = (fixedParameters[i] == 0.0);
  }
  if (nullState)
  {
    this->SetDisplacementField(static_cast<DisplacementFieldType *>(nullptr));
    this->SetInverseDisplacementField(nullptr);
    return;
  }

  SizeType size;
  for (unsigned int d = 0; d < VDimension; ++d)
  {
    size[d] = static_cast<SizeValueType>(fixedParameters[d]);
  }

  PointType origin;
  for (unsigned int d = 0; d < VDimension; ++d)
  {
    origin[d] = fixedParameters[d + VDimension];
  }

  SpacingType spacing;
  for (unsigned int d = 0; d < VDimension; ++d)
  {
    spacing[d] = fixedParameters[d + 2 * VDimension];
  }

  DirectionType direction;
  for (unsigned int di = 0; di < VDimension; ++di)
  {
    for (unsigned int dj = 0; dj < VDimension; ++dj)
    {
      direction[di][dj] = fixedParameters[3 * VDimension + (di * VDimension + dj)];
    }
  }

  PixelType zeroDisplacement;
  zeroDisplacement.Fill(0.0);

  auto displacementField = DisplacementFieldType::New();
  displacementField->SetSpacing(spacing);
  displacementField->SetOrigin(origin);
  displacementField->SetDirection(direction);
  displacementField->SetRegions(size);
  displacementField->Allocate();
  displacementField->FillBuffer(zeroDisplacement);

  this->SetDisplacementField(displacementField);

  if (!this->m_InverseDisplacementField.IsNull())
  {
    auto inverseDisplacementField = DisplacementFieldType::New();
    inverseDisplacementField->SetSpacing(spacing);
    inverseDisplacementField->SetOrigin(origin);
    inverseDisplacementField->SetDirection(direction);
    inverseDisplacementField->SetRegions(size);
    inverseDisplacementField->Allocate();
    inverseDisplacementField->FillBuffer(zeroDisplacement);

    this->SetInverseDisplacementField(inverseDisplacementField);
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::SetFixedParametersFromDisplacementField() const
{
  this->m_FixedParameters.SetSize(VDimension * (VDimension + 3));

  if (this->m_DisplacementField.IsNull())
  {
    this->m_FixedParameters.Fill(0.0);
    return;
  }

  const typename DisplacementFieldType::RegionType & fieldRegion =
    this->m_DisplacementField->GetLargestPossibleRegion();

  // Set the field size parameters
  SizeType fieldSize = fieldRegion.GetSize();
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    this->m_FixedParameters[i] = static_cast<FixedParametersValueType>(fieldSize[i]);
  }

  // Set the origin parameters
  PointType fieldOrigin = this->m_DisplacementField->GetOrigin();
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    this->m_FixedParameters[VDimension + i] = fieldOrigin[i];
  }

  // Set the spacing parameters
  SpacingType fieldSpacing = this->m_DisplacementField->GetSpacing();
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    this->m_FixedParameters[2 * VDimension + i] = static_cast<FixedParametersValueType>(fieldSpacing[i]);
  }

  // Set the direction parameters
  DirectionType fieldDirection = this->m_DisplacementField->GetDirection();
  for (unsigned int di = 0; di < VDimension; ++di)
  {
    for (unsigned int dj = 0; dj < VDimension; ++dj)
    {
      this->m_FixedParameters[3 * VDimension + (di * VDimension + dj)] =
        static_cast<FixedParametersValueType>(fieldDirection[di][dj]);
    }
  }
}

template <typename TParametersValueType, unsigned int VDimension>
void
DisplacementFieldTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(DisplacementField);
  itkPrintSelfObjectMacro(InverseDisplacementField);

  itkPrintSelfObjectMacro(Interpolator);
  itkPrintSelfObjectMacro(InverseInterpolator);

  os << indent << "DisplacementFieldSetTime: "
     << static_cast<typename NumericTraits<ModifiedTimeType>::PrintType>(m_DisplacementFieldSetTime) << std::endl;

  os << indent
     << "m_IdentityJacobian: " << static_cast<typename NumericTraits<JacobianType>::PrintType>(m_IdentityJacobian)
     << std::endl;

  os << indent << " CoordinateTolerance: " << m_CoordinateTolerance << std::endl;
  os << indent << " DirectionTolerance: " << m_DirectionTolerance << std::endl;
}
} // namespace itk

#endif
