/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianGoodnessOfFitComponent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianGoodnessOfFitComponent_h
#define __itkGaussianGoodnessOfFitComponent_h

#include "itkGoodnessOfFitComponentBase.h"
#include "itkGaussianDensityFunction.h"
#include "itkFunctionBase.h"
#include "itkWeightedCovarianceCalculator.h"
#include "itkSymmetricEigenSystem.h"

namespace itk{ 
namespace Statistics{

/** \class GaussianGoodnessOfFitComponent 
 *  \brief provides implemenations of GoodnessOfFitComponentBase's methods
 * for a Gaussian component
 *
 * Among the GoodnessOfFitComponentBase's methods, this class provides
 * implementations for the CalculateProjectionAxess, the GetCumulativeProbability
 * (univariate CDF), and the GetProbabilityDensity (multivariate PDF)methods.
 *
 * The CalculateProjectionAxes method creats an array of projection axes that
 * are the eigen vectors generated from the weighted covariance matrix of the
 * resampled sample using a spherical kernel.
 *
 * \sa GoodnessOfFitComponentBase, GoodnessOfFitMixtureModelCostFunction
 */

template< class TInputSample >
class GaussianGoodnessOfFitComponent 
  : public GoodnessOfFitComponentBase< TInputSample > 
{
public:
  /** Standard class typedefs */
  typedef GaussianGoodnessOfFitComponent Self;
  typedef GoodnessOfFitComponentBase< TInputSample > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianGoodnessOfFitComponent, 
               GoodnessOfFitComponentBase) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedefs from input sample */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TInputSample::MeasurementVectorSize) ;
  typedef typename TInputSample::MeasurementType MeasurementType ;
  typedef typename TInputSample::MeasurementVectorType MeasurementVectorType ;

  /** typedefs from Superclass */
  typedef typename Superclass::CenterType CenterType ;
  typedef typename Superclass::RadiusType RadiusType ;
  typedef typename Superclass::MeanType MeanType ;
  typedef typename Superclass::StandardDeviationType StandardDeviationType ;
  typedef typename Superclass::ResampledSampleType ResampledSampleType ;
  typedef typename Superclass::ParametersType ParametersType ;

  /** Weight function type. The density values are used as weights of 
   * each instance (measurement vector) for the Covariance calulator */
  typedef GaussianDensityFunction< MeasurementVectorType > 
  ProbabilityDensityFunctionType ;

  typedef typename ProbabilityDensityFunctionType::CovarianceType CovarianceType ;

  /** Covariance calculator type. the output of this calculator is
   * a covariance matrix that is used as the input of the Projection 
   * calculator */
  typedef WeightedCovarianceCalculator< ResampledSampleType > 
  CovarianceCalculatorType ;

  /** Default projection axis calculator type*/
  typedef SymmetricEigenSystem< double, itkGetStaticConstMacro(MeasurementVectorSize) > 
  ProjectionAxisCalculatorType ;

  /** Gets the size of parameters which consists of mean
   * and standard deviation */
  unsigned int GetNumberOfParameters() const
  { return (unsigned int)(itkGetStaticConstMacro(MeasurementVectorSize) + 1) ; }

  /** Sets the component distribution parameters */
  void SetParameters(const ParametersType &parameter) ;

  CenterType* GetCenter() ;

  RadiusType* GetRadius() ;
  
  MeanType* GetMean() ;

  StandardDeviationType* GetStandardDeviation() ;

  /** Univariate (standard) cumulative probability function */
  double GetCumulativeProbability(double x) const ;

  /** Multivariate probability density function */
  double GetProbabilityDensity(MeasurementVectorType &measurements) const ;

  void PrintParameters(std::ostream &os) const ;

  /** Gets the full distribution parameters which consists of
   * mean vector and covariance matrix in a single array */
  ParametersType GetFullParameters() const ;

protected:
  GaussianGoodnessOfFitComponent() ;
  virtual ~GaussianGoodnessOfFitComponent() ;

  /** Calculates the base axes for projection */
  virtual void CalculateProjectionAxes() ;

private:
  typename ProbabilityDensityFunctionType::Pointer m_ProbabilityDensityFunction ;

  typename CovarianceCalculatorType::Pointer m_CovarianceCalculator ;

  typename ProjectionAxisCalculatorType::Pointer m_ProjectionAxisCalculator ;

  MeanType m_Mean ;
  CenterType m_Center ;
  RadiusType m_Radius ;
  StandardDeviationType m_StandardDeviation ;
  CovarianceType m_Covariance ;

  unsigned int m_NumberOfParameters ;

  int m_LongestAxisIndex ;
  double m_LargestEigenValue ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianGoodnessOfFitComponent.txx"
#endif

#endif

