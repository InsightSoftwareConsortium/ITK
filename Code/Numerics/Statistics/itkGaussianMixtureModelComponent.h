/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianMixtureModelComponent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianMixtureModelComponent_h
#define __itkGaussianMixtureModelComponent_h

#include "itkMixtureModelComponentBase.h"
#include "itkGaussianDensityFunction.h"
#include "itkWeightedMeanCalculator.h"
#include "itkWeightedCovarianceCalculator.h"

namespace itk{ 
namespace Statistics{
  
/** \class GaussianMixtureModelComponent
 * \brief calculates sample mean
 *
 * You plug in the target sample data using SetSample method. Then call
 * the GenerateData method to run the alogithm.
 *
 * The return value that the GetOutput method 
 * \f$ = \frac{1}{n}\sum^{n}_{i=1} \f$ where \f$n\f$ is the
 * number of measurement vectors in the target 
 *
 */

template< class TSample >
class GaussianMixtureModelComponent :
    public MixtureModelComponentBase< TSample >
{
public:
  /**Standard class typedefs. */
  typedef GaussianMixtureModelComponent Self;
  typedef MixtureModelComponentBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /**Standard Macros */
  itkTypeMacro(GaussianMixtureModelComponent, MixtureModelComponentBase);
  itkNewMacro(Self) ;

  /** Measurement length constant */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::MembershipFunctionType MembershipFunctionType ;
  typedef typename Superclass::WeightArrayType WeightArrayType ;
  typedef typename Superclass::ParametersType ParametersType ;

  typedef GaussianDensityFunction< MeasurementVectorType > NativeMembershipFunctionType ;
  
  typedef WeightedMeanCalculator< TSample > MeanEstimatorType ;
  typedef WeightedCovarianceCalculator< TSample > CovarianceEstimatorType ;

  typedef typename MeanEstimatorType::OutputType MeanType ;
  typedef typename CovarianceEstimatorType::OutputType CovarianceType ;

  void SetSample(TSample* sample) ;

  void SetParameters(ParametersType &parameters) ;
  
protected:
  GaussianMixtureModelComponent() ;
  virtual ~GaussianMixtureModelComponent() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  double CalculateParametersChange() ;

  void GenerateData() ;

private:
  typename NativeMembershipFunctionType::Pointer m_GaussianDensityFunction ;
  MeanType m_Mean ;
  CovarianceType m_Covariance ;
  typename MeanEstimatorType::Pointer m_MeanEstimator ;
  typename CovarianceEstimatorType::Pointer m_CovarianceEstimator ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianMixtureModelComponent.txx"
#endif

#endif

