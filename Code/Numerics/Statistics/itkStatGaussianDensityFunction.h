/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatGaussianDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkStatGaussianDensityFunction_h
#define __itkStatGaussianDensityFunction_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_transpose.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_determinant.h>


#include "itkStatDensityFunction.h"

#define PI 3.141592

namespace itk{

/** \class GaussianDensityFunction
 * \brief GaussianDensityFunction class represents Gaussian Density Function.
 *
 * This class keeps parameter to define Gaussian Density Function  and has
 * method to return the probability (the value of Gaussian Density Function) 
 * of an instance.  FeatureDimension is the dimension of feature space.
 * double is type of feature. 
 */

template <class TFeatureCoorRep = float, unsigned int VFeatureDimension>
class ITK_EXPORT GaussianDensityFunction 
: public DensityFunction<TFeatureCoorRep, VFeatureDimension>
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef GaussianDensityFunction Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;

  typedef TFeatureCoorRep FeatureCoorRepType;

 /**
  * Standard Superclass typedef
  */
  typedef DensityFunction<FeatureCoorRepType, VFeatureDimension> Superclass;

  typedef typename Superclass::FeatureType FeatureType;

 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information
  */
  itkTypeMacro(GaussianDensityFunction,DensityFunction);

 /**
  * Method to get mean
  */
  vnl_vector<double> GetMeans() 
  { return m_Means; };

 /**
  * Method to set mean
  */
  void SetMeans(vnl_vector<double> means) 
  { m_Means = means; };

 /**
  * Method to get standard deviation
  */
  vnl_vector<double> GetStandardDeviations() 
  { return m_StandardDeviations; };

 /**
  * Method to set standard deviation
  */
  void SetStandardDeviation(vnl_vector<double> std) 
  { m_StandardDeviation = std; };

 /**
  * Method to get covariance matrix
  */
  vnl_matrix<double> GetCovariance() 
  { return m_Covariance; };
 
 /**
  * Method to set covariance matrix
  * Also, this function calculates inverse covariance and pre factor of 
  * Gaussian Distribution to speed up GetProbability
  */
  void SetCovariance(vnl_matrix<double> cov); 
  

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability.
  */
  double GetDensity(FeatureType feature);
  

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability.
  */
  double GetDensity(double feature);
  

protected:		
  GaussianDensityFunction(void){};
  ~GaussianDensityFunction(void){};

  vnl_vector<double>  m_Means;              // mean
  vnl_vector<double>  m_StandardDeviations; // standard deviation
  vnl_matrix<double>  m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()
  vnl_matrix<double>  m_InverseCovariance; 

  // pre_factor which is automatically calculated 
  // when covariace matirx is set.  This speeds up the GetProbability()  
  double m_PreFactor;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatGaussianDensityFunction.txx"
#endif

#endif
