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
#ifndef itkKappaStatisticImageToImageMetric_hxx
#define itkKappaStatisticImageToImageMetric_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{

template <typename TFixedImage, typename TMovingImage>
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::KappaStatisticImageToImageMetric()
  : m_ForegroundValue(255)

{
  this->SetComputeGradient(true);
}

template <typename TFixedImage, typename TMovingImage>
auto
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::GetValue(const TransformParametersType & parameters) const
  -> MeasureType
{
  itkDebugMacro("GetValue( " << parameters << " ) ");

  this->SetTransformParameters(parameters);

  // Get the fixed image
  //
  const FixedImageConstPointer fixedImage = this->m_FixedImage;
  if (!fixedImage)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  // Get an iterator over the fixed image
  //
  using FixedIteratorType = ImageRegionConstIteratorWithIndex<FixedImageType>;
  typename FixedImageType::IndexType fixedIndex;
  FixedIteratorType                  fi(fixedImage, fixedImage->GetBufferedRegion());

  // Get the moving image
  //
  const MovingImageConstPointer movingImage = this->m_MovingImage;
  if (!movingImage)
  {
    itkExceptionMacro("Moving image has not been assigned");
  }

  // The metric computation requires using the following:
  // - 'measure': the value of the metric.
  // - 'fixedForegroundArea': the total area of the foreground region in the
  // fixed image.
  // - 'movingForegroundArea': the foreground area in the moving image in the
  // area of overlap under the current transformation.
  // - 'intersection': the area of foreground intersection between the fixed
  // and moving image.
  //
  MeasureType measure;
  MeasureType intersection{};
  MeasureType movingForegroundArea{};
  MeasureType fixedForegroundArea{};

  // Compute fixedForegroundArea, movingForegroundArea, and
  // intersection. Loop over the fixed image.
  //
  while (!fi.IsAtEnd())
  {
    fixedIndex = fi.GetIndex();

    InputPointType fixedInputPoint;
    fixedImage->TransformIndexToPhysicalPoint(fixedIndex, fixedInputPoint);

    if (this->m_FixedImageMask && !this->m_FixedImageMask->IsInsideInWorldSpace(fixedInputPoint))
    {
      ++fi;
      continue;
    }

    const RealType fixedValue = fi.Get();

    // Increment 'fixedForegroundArea'
    //
    //
    if (Math::AlmostEquals(fixedValue, m_ForegroundValue))
    {
      ++fixedForegroundArea;
    }

    // Get the point in the transformed moving image corresponding to
    // the point in the fixed image (physical coordinates)
    //
    //
    const OutputPointType transformedPoint = this->m_Transform->TransformPoint(fixedInputPoint);

    if (this->m_MovingImageMask && !this->m_MovingImageMask->IsInsideInWorldSpace(transformedPoint))
    {
      ++fi;
      continue;
    }

    // Compute movingForegroundArea and intersection
    //
    //
    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);
      if (Math::AlmostEquals(movingValue, m_ForegroundValue))
      {
        ++movingForegroundArea;
      }
      if (Math::AlmostEquals(movingValue, m_ForegroundValue) && Math::AlmostEquals(fixedValue, m_ForegroundValue))
      {
        ++intersection;
      }
    }
    ++fi;
  }

  // Compute the final metric value
  //
  //
  if (!m_Complement)
  {
    measure = 2.0 * (intersection) / (fixedForegroundArea + movingForegroundArea);
  }
  else
  {
    measure = 1.0 - 2.0 * (intersection) / (fixedForegroundArea + movingForegroundArea);
  }

  return measure;
}

template <typename TFixedImage, typename TMovingImage>
void
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::GetDerivative(const TransformParametersType & parameters,
                                                                           DerivativeType & derivative) const
{
  itkDebugMacro("GetDerivative( " << parameters << " ) ");

  if (!this->GetGradientImage())
  {
    itkExceptionMacro("The gradient image is null, maybe you forgot to call Initialize()");
  }

  const FixedImageConstPointer fixedImage = this->m_FixedImage;
  if (!fixedImage)
  {
    itkExceptionMacro("Fixed image has not been assigned");
  }

  const unsigned int ImageDimension = FixedImageType::ImageDimension;

  using FixedIteratorType = ImageRegionConstIteratorWithIndex<FixedImageType>;

  FixedIteratorType ti(fixedImage, this->GetFixedImageRegion());

  typename FixedImageType::IndexType index;

  this->m_NumberOfPixelsCounted = 0;

  this->SetTransformParameters(parameters);

  const unsigned int ParametersDimension = this->GetNumberOfParameters();
  derivative = DerivativeType(ParametersDimension);
  derivative.Fill(typename DerivativeType::ValueType{});

  using ArrayType = Array<double>;

  ArrayType sum1(ParametersDimension);
  sum1.Fill(typename ArrayType::ValueType{});

  ArrayType sum2(ParametersDimension);
  sum2.Fill(typename ArrayType::ValueType{});

  int fixedArea = 0;
  int movingArea = 0;
  int intersection = 0;

  TransformJacobianType jacobian(TFixedImage::ImageDimension, this->m_Transform->GetNumberOfParameters());
  TransformJacobianType jacobianCache;

  ti.GoToBegin();
  while (!ti.IsAtEnd())
  {
    index = ti.GetIndex();

    InputPointType inputPoint;
    fixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

    if (this->m_FixedImageMask && !this->m_FixedImageMask->IsInsideInWorldSpace(inputPoint))
    {
      ++ti;
      continue;
    }

    const RealType fixedValue = ti.Value();
    if (Math::AlmostEquals(fixedValue, m_ForegroundValue))
    {
      ++fixedArea;
    }

    const OutputPointType transformedPoint = this->m_Transform->TransformPoint(inputPoint);

    if (this->m_MovingImageMask && !this->m_MovingImageMask->IsInsideInWorldSpace(transformedPoint))
    {
      ++ti;
      continue;
    }

    if (this->m_Interpolator->IsInsideBuffer(transformedPoint))
    {
      const RealType movingValue = this->m_Interpolator->Evaluate(transformedPoint);

      if (Math::AlmostEquals(movingValue, m_ForegroundValue))
      {
        ++movingArea;
      }

      if (Math::AlmostEquals(movingValue, m_ForegroundValue) && Math::AlmostEquals(fixedValue, m_ForegroundValue))
      {
        ++intersection;
      }

      this->m_Transform->ComputeJacobianWithRespectToParametersCachedTemporaries(inputPoint, jacobian, jacobianCache);

      this->m_NumberOfPixelsCounted++;

      // Get the gradient by NearestNeighborInterpolation:
      // which is equivalent to round up the point components.
      using CoordinateType = typename OutputPointType::CoordinateType;
      using MovingImageContinuousIndexType = ContinuousIndex<CoordinateType, MovingImageType::ImageDimension>;

      const MovingImageContinuousIndexType tempIndex =
        this->m_MovingImage->template TransformPhysicalPointToContinuousIndex<CoordinateType>(transformedPoint);

      typename MovingImageType::IndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);

      const GradientPixelType gradient = this->m_GradientImage->GetPixel(mappedIndex);
      for (unsigned int par = 0; par < ParametersDimension; ++par)
      {
        for (unsigned int dim = 0; dim < ImageDimension; ++dim)
        {
          sum2[par] += jacobian(dim, par) * gradient[dim];
          if (Math::AlmostEquals(fixedValue, m_ForegroundValue))
          {
            sum1[par] += 2.0 * jacobian(dim, par) * gradient[dim];
          }
        }
      }
    }
    ++ti;
  }

  if (!this->m_NumberOfPixelsCounted)
  {
    itkExceptionMacro("All the points mapped to outside of the moving image");
  }
  else
  {
    const double areaSum = static_cast<double>(fixedArea) + static_cast<double>(movingArea);
    for (unsigned int par = 0; par < ParametersDimension; ++par)
    {
      derivative[par] = -(areaSum * sum1[par] - 2.0 * intersection * sum2[par]) / (areaSum * areaSum);
    }
  }
}

template <typename TFixedImage, typename TMovingImage>
void
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::ComputeGradient()
{
  const unsigned int dim = MovingImageType::ImageDimension;

  auto tempGradientImage = GradientImageType::New();
  tempGradientImage->SetRegions(this->m_MovingImage->GetBufferedRegion().GetSize());
  tempGradientImage->Allocate();
  tempGradientImage->Update();

  using GradientIteratorType = ImageRegionIteratorWithIndex<GradientImageType>;
  using MovingIteratorType = ImageRegionConstIteratorWithIndex<MovingImageType>;

  GradientIteratorType git(tempGradientImage, tempGradientImage->GetBufferedRegion());
  MovingIteratorType   mit(this->m_MovingImage, this->m_MovingImage->GetBufferedRegion());

  git.GoToBegin();
  mit.GoToBegin();

  typename MovingImageType::IndexType   minusIndex;
  typename MovingImageType::IndexType   plusIndex;
  typename MovingImageType::IndexType   currIndex;
  typename GradientImageType::PixelType tempGradPixel;
  typename MovingImageType::SizeType    movingSize = this->m_MovingImage->GetBufferedRegion().GetSize();
  while (!mit.IsAtEnd())
  {
    currIndex = mit.GetIndex();
    minusIndex = mit.GetIndex();
    plusIndex = mit.GetIndex();
    for (unsigned int i = 0; i < dim; ++i)
    {
      if ((currIndex[i] == 0) ||
          (static_cast<typename MovingImageType::SizeType::SizeValueType>(currIndex[i]) == (movingSize[i] - 1)))
      {
        tempGradPixel[i] = 0;
      }
      else
      {
        minusIndex[i] = currIndex[i] - 1;
        plusIndex[i] = currIndex[i] + 1;
        auto minusVal = static_cast<double>(this->m_MovingImage->GetPixel(minusIndex));
        auto plusVal = static_cast<double>(this->m_MovingImage->GetPixel(plusIndex));
        if (Math::NotAlmostEquals(minusVal, m_ForegroundValue) && Math::AlmostEquals(plusVal, m_ForegroundValue))
        {
          tempGradPixel[i] = 1;
        }
        else if (Math::AlmostEquals(minusVal, m_ForegroundValue) && Math::NotAlmostEquals(plusVal, m_ForegroundValue))
        {
          tempGradPixel[i] = -1;
        }
        else
        {
          tempGradPixel[i] = 0;
        }
      }
      minusIndex = currIndex;
      plusIndex = currIndex;
    }
    git.Set(tempGradPixel);
    ++git;
    ++mit;
  }

  this->m_GradientImage = tempGradientImage;
}

template <typename TFixedImage, typename TMovingImage>
void
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::GetValueAndDerivative(
  const TransformParametersType & parameters,
  MeasureType &                   Value,
  DerivativeType &                Derivative) const
{
  Value = this->GetValue(parameters);
  this->GetDerivative(parameters, Derivative);
}

template <typename TFixedImage, typename TMovingImage>
void
KappaStatisticImageToImageMetric<TFixedImage, TMovingImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(Complement);
  os << indent << "ForegroundValue: " << m_ForegroundValue << std::endl;
}

} // end namespace itk

#endif
