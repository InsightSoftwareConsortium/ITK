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
#ifndef itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_hxx
#define itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_hxx

#include "itkMath.h"

namespace itk
{

template <typename TPointSet, class TInternalComputationValueType>
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::
  JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4()
  : m_PointSetSigma(static_cast<RealType>(1.0))
  , m_KernelSigma(static_cast<RealType>(10.0))
  , m_CovarianceKNeighborhood(5U)
  , m_EvaluationKNeighborhood(50U)
  , m_Alpha(static_cast<RealType>(1.0))
  , m_TotalNumberOfPoints(0)
  , m_Prefactor0(0.0)
  , m_Prefactor1(0.0)
{}

template <typename TPointSet, class TInternalComputationValueType>
void
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::Initialize()
{
  Superclass::Initialize();

  // Initialize the moving density function
  this->m_MovingDensityFunction = DensityFunctionType::New();
  this->m_MovingDensityFunction->SetKernelSigma(this->m_KernelSigma);
  this->m_MovingDensityFunction->SetRegularizationSigma(this->m_PointSetSigma);
  this->m_MovingDensityFunction->SetNormalize(true);
  this->m_MovingDensityFunction->SetUseAnisotropicCovariances(this->m_UseAnisotropicCovariances);
  this->m_MovingDensityFunction->SetCovarianceKNeighborhood(this->m_CovarianceKNeighborhood);
  this->m_MovingDensityFunction->SetEvaluationKNeighborhood(this->m_EvaluationKNeighborhood);
  this->m_MovingDensityFunction->SetInputPointSet(this->m_MovingTransformedPointSet);

  // Pre-calculate some values for efficiency
  this->m_TotalNumberOfPoints = static_cast<RealType>(
    this->m_NumberOfValidPoints + this->m_MovingDensityFunction->GetInputPointSet()->GetNumberOfPoints());
  this->m_Prefactor0 = -1.0 / static_cast<RealType>(this->m_TotalNumberOfPoints);
  if (this->m_Alpha != 1.0)
  {
    this->m_Prefactor0 /= (this->m_Alpha - 1.0);
  }
  this->m_Prefactor1 = 1.0 / (this->m_TotalNumberOfPoints * this->m_TotalNumberOfPoints);
}

template <typename TPointSet, class TInternalComputationValueType>
typename JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::MeasureType
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::
  GetLocalNeighborhoodValue(const PointType & point, const PixelType & itkNotUsed(pixel)) const
{
  MeasureType         value;
  LocalDerivativeType derivative;
  this->ComputeValueAndDerivative(point, value, derivative, true, false);
  return value;
}

template <typename TPointSet, class TInternalComputationValueType>
void
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::
  GetLocalNeighborhoodValueAndDerivative(const PointType &     point,
                                         MeasureType &         value,
                                         LocalDerivativeType & derivative,
                                         const PixelType &     itkNotUsed(pixel)) const
{
  this->ComputeValueAndDerivative(point, value, derivative, true, true);
}

template <typename TPointSet, class TInternalComputationValueType>
void
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::
  ComputeValueAndDerivative(const PointType &     samplePoint,
                            MeasureType &         value,
                            LocalDerivativeType & derivativeReturn,
                            bool                  calcValue,
                            bool                  calcDerivative) const
{
  if (calcDerivative)
  {
    derivativeReturn.Fill(DerivativeValueType{});
  }
  value = MeasureType{};

  /**
   * first term only
   */
  typename PointSetType::PointIdentifier numberOfMovingPoints =
    this->m_MovingDensityFunction->GetInputPointSet()->GetNumberOfPoints();
  RealType probabilityStar =
    this->m_MovingDensityFunction->Evaluate(samplePoint) * static_cast<RealType>(numberOfMovingPoints);

  probabilityStar /= this->m_TotalNumberOfPoints;

  if (Math::AlmostEquals(probabilityStar, RealType{}))
  {
    return;
  }

  if (calcValue)
  {
    RealType realOne = NumericTraits<RealType>::OneValue();
    if (Math::AlmostEquals(this->m_Alpha, realOne))
    {
      value = (std::log(probabilityStar));
    }
    else
    {
      value = realOne * (std::pow(probabilityStar, static_cast<RealType>(this->m_Alpha - realOne)));
    }
    value *= this->m_Prefactor0;
  }

  if (calcDerivative)
  {
    RealType probabilityStarFactor = std::pow(probabilityStar, static_cast<RealType>(2.0 - this->m_Alpha));

    typename DensityFunctionType::NeighborsIdentifierType neighbors;
    this->m_MovingDensityFunction->GetPointsLocator()->FindClosestNPoints(
      samplePoint, this->m_EvaluationKNeighborhood, neighbors);

    for (SizeValueType n = 0; n < neighbors.size(); ++n)
    {
      RealType gaussian = this->m_MovingDensityFunction->GetGaussian(neighbors[n])->Evaluate(samplePoint);

      if (Math::AlmostEquals(gaussian, RealType{}))
      {
        continue;
      }

      typename GaussianType::MeanVectorType mean = this->m_MovingDensityFunction->GetGaussian(neighbors[n])->GetMean();

      Array<CoordRepType> diffMean(PointDimension);
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        diffMean[i] = mean[i] - samplePoint[i];
      }

      if (this->m_UseAnisotropicCovariances)
      {
        typename GaussianType::CovarianceMatrixType Ci =
          this->m_MovingDensityFunction->GetGaussian(neighbors[n])->GetInverseCovariance();
        diffMean = Ci * diffMean;
      }
      else
      {
        diffMean /= this->m_MovingDensityFunction->GetGaussian(neighbors[n])->GetCovariance()(0, 0);
      }

      DerivativeValueType factor = this->m_Prefactor1 * gaussian / probabilityStarFactor;
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        derivativeReturn[i] += diffMean[i] * factor;
      }
    }
  }
}

template <typename TPointSet, class TInternalComputationValueType>
typename LightObject::Pointer
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::InternalClone() const
{
  auto rval = Self::New();
  rval->SetMovingPointSet(this->m_MovingPointSet);
  rval->SetFixedPointSet(this->m_FixedPointSet);
  rval->SetPointSetSigma(this->m_PointSetSigma);
  rval->SetEvaluationKNeighborhood(this->m_EvaluationKNeighborhood);
  rval->SetAlpha(this->m_Alpha);
  rval->SetKernelSigma(this->m_KernelSigma);
  rval->SetCovarianceKNeighborhood(this->m_CovarianceKNeighborhood);
  rval->SetUseAnisotropicCovariances(this->m_UseAnisotropicCovariances);

  return rval.GetPointer();
}

template <typename TPointSet, class TInternalComputationValueType>
void
JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4<TPointSet, TInternalComputationValueType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(UseAnisotropicCovariances);

  os << indent << "PointSetSigma: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_PointSetSigma)
     << std::endl;
  os << indent << "KernelSigma: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_KernelSigma)
     << std::endl;
  os << indent << "CovarianceKNeighborhood: " << m_CovarianceKNeighborhood << std::endl;
  os << indent << "EvaluationKNeighborhood: " << m_EvaluationKNeighborhood << std::endl;

  os << indent << "Alpha: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_Alpha) << std::endl;

  os << indent
     << "TotalNumberOfPoints: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_TotalNumberOfPoints)
     << std::endl;
  os << indent << "Prefactor0: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_Prefactor0) << std::endl;
  os << indent << "Prefactor1: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_Prefactor1) << std::endl;
}
} // end namespace itk

#endif
