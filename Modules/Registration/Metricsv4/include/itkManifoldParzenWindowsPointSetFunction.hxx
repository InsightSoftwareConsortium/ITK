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
#ifndef itkManifoldParzenWindowsPointSetFunction_hxx
#define itkManifoldParzenWindowsPointSetFunction_hxx

#include "itkManifoldParzenWindowsPointSetFunction.h"

#include "itkMath.h"
#include "itkCompensatedSummation.h"

namespace itk
{

template <typename TPointSet, typename TOutput, typename TCoordRep>
ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::ManifoldParzenWindowsPointSetFunction()
  : m_PointsLocator(nullptr)
  , m_RegularizationSigma(1.0)
  , m_KernelSigma(1.0)

{
  m_MultiThreader = MultiThreaderBase::New();
}

template <typename TPointSet, typename TOutput, typename TCoordRep>
void
ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::SetInputPointSet(const InputPointSetType * ptr)
{
  this->m_PointSet = ptr;

  /**
   * Generate KdTree and create set of gaussians from input point set
   */
  std::vector<typename GaussianType::Pointer> inputGaussians;
  inputGaussians.resize(this->GetInputPointSet()->GetNumberOfPoints());
  this->m_Gaussians.resize(this->GetInputPointSet()->GetNumberOfPoints());

  const PointsContainer * points = this->GetInputPointSet()->GetPoints();

  m_MultiThreader->ParallelizeArray(
    0,
    points->Size(),
    [&](SizeValueType index) {
      CovarianceMatrixType covariance(PointDimension, PointDimension);

      covariance.SetIdentity();
      covariance *= this->m_KernelSigma;

      inputGaussians[index] = GaussianType::New();
      inputGaussians[index]->SetMeasurementVectorSize(PointDimension);
      inputGaussians[index]->SetMean(points->ElementAt(index));
      inputGaussians[index]->SetCovariance(covariance);
    },
    nullptr);

  this->m_PointsLocator = PointsLocatorType::New();
  this->m_PointsLocator->SetPoints(const_cast<PointsContainer *>(points));
  this->m_PointsLocator->Initialize();

  /**
   * Calculate covariance matrices
   */
  m_MultiThreader->ParallelizeArray(
    0,
    points->Size(),
    [&](SizeValueType index) {
      PointType point = points->ElementAt(index);

      this->m_Gaussians[index] = GaussianType::New();
      this->m_Gaussians[index]->SetMeasurementVectorSize(PointDimension);
      this->m_Gaussians[index]->SetMean(inputGaussians[index]->GetMean());

      if (this->m_CovarianceKNeighborhood > 0 && this->m_UseAnisotropicCovariances)
      {
        CovarianceMatrixType Cout(PointDimension, PointDimension);
        Cout.Fill(0);

        typename PointsLocatorType::NeighborsIdentifierType neighbors;
        this->m_PointsLocator->Search(point, this->m_CovarianceKNeighborhood, neighbors);

        CompensatedSummation<RealType> denominator;
        for (unsigned int j = 0; j < this->m_CovarianceKNeighborhood; j++)
        {
          if (neighbors[j] != index && neighbors[j] < this->GetInputPointSet()->GetNumberOfPoints())
          {
            PointType neighbor = this->GetInputPointSet()->GetPoint(neighbors[j]);

            RealType kernelValue = inputGaussians[index]->Evaluate(neighbor);

            denominator += kernelValue;
            if (kernelValue > 0.0)
            {
              for (unsigned int m = 0; m < PointDimension; m++)
              {
                for (unsigned int n = m; n < PointDimension; n++)
                {
                  RealType covariance = kernelValue * (neighbor[m] - point[m]) * (neighbor[n] - point[n]);
                  Cout(m, n) += covariance;
                  Cout(n, m) += covariance;
                }
              }
            }
          }
        }

        if (this->m_Normalize && denominator.GetSum() > 0.0)
        {
          Cout /= denominator.GetSum();
        }
        else
        {
          Cout /= static_cast<RealType>(this->m_CovarianceKNeighborhood);
        }
        for (unsigned int m = 0; m < PointDimension; m++)
        {
          Cout(m, m) += Math::sqr(this->m_RegularizationSigma);
        }

        this->m_Gaussians[index]->SetCovariance(Cout);
      }
      else
      {
        typename GaussianType::CovarianceMatrixType covariance(PointDimension, PointDimension);
        covariance.SetIdentity();
        covariance *= Math::sqr(this->m_RegularizationSigma);
        this->m_Gaussians[index]->SetCovariance(covariance);
      }
    },
    nullptr);
}

template <typename TPointSet, typename TOutput, typename TCoordRep>
TOutput
ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::Evaluate(const InputPointType & point) const
{
  if (this->GetInputPointSet() == nullptr)
  {
    itkExceptionMacro("The input point set has not been specified.");
  }

  unsigned int numberOfNeighbors =
    std::min(this->m_EvaluationKNeighborhood, static_cast<unsigned int>(this->m_Gaussians.size()));

  CompensatedSummation<OutputType> sum;

  if (numberOfNeighbors == this->m_Gaussians.size())
  {
    for (unsigned int j = 0; j < this->m_Gaussians.size(); j++)
    {
      sum += static_cast<OutputType>(this->m_Gaussians[j]->Evaluate(point));
    }
  }
  else
  {
    typename PointsLocatorType::NeighborsIdentifierType neighbors;
    this->m_PointsLocator->Search(point, numberOfNeighbors, neighbors);

    for (unsigned int j = 0; j < numberOfNeighbors; j++)
    {
      sum += static_cast<OutputType>(this->m_Gaussians[neighbors[j]]->Evaluate(point));
    }
  }
  return static_cast<OutputType>(sum.GetSum() / static_cast<OutputType>(this->m_Gaussians.size()));
}

template <typename TPointSet, typename TOutput, typename TCoordRep>
typename ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::GaussianConstPointer
ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::GetGaussian(PointIdentifier i) const
{
  if (i < this->m_Gaussians.size())
  {
    return this->m_Gaussians[i].GetPointer();
  }
  else
  {
    return nullptr;
  }
}

/**
 * Standard "PrintSelf" method
 */
template <typename TPointSet, typename TOutput, typename TCoordRep>
void
ManifoldParzenWindowsPointSetFunction<TPointSet, TOutput, TCoordRep>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Covariance neighborhood: " << this->m_CovarianceKNeighborhood << std::endl;
  os << indent << "Evaluation neighborhood: " << this->m_EvaluationKNeighborhood << std::endl;
  os << indent << "Regularization sigma: " << this->m_RegularizationSigma << std::endl;
  os << indent << "Kernel sigma: " << this->m_KernelSigma << std::endl;
  os << indent << "Normalize: " << this->m_Normalize << std::endl;
  os << indent << "Use anisotropic covariances: " << this->m_UseAnisotropicCovariances << std::endl;
}

} // end namespace itk

#endif
