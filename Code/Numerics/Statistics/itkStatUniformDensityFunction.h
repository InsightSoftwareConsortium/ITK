/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatUniformDensityFunction.h
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

#ifndef __itkStatUniformDensityFunction_h
#define __itkStatUniformDensityFunction_h

#include <vnl/vnl_vector.h>

#include "itkStatDensityFunction.h"


namespace itk{

/** \class UniformDensityFunction
 * \brief This class expresses uniform density distribution 
 *
 * This class keeps parameter to define Uniform Density Function  and has
 * method to return the probability (the value of Uniform Density Function) 
 * of an instance.  FeatureDimension is the dimension of feature space.
 * double is type of feature. (e.g. For RGB data, double can be
 * 3-dimensional vector.  For black and white image, TFeture can be scalar)
 */

template <class TFeatureCoorRep = float, unsigned int VFeatureDimension>
class ITK_EXPORT UniformDensityFunction 
: public DensityFunction<TFeatureCoorRep, VFeatureDimension>
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef UniformDensityFunction Self;

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
  * Dimension of the feature vector
  */
  enum { FeatureDimension = VFeatureDimension };

 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information
  */
  itkTypeMacro(UniformDensityFunction,DensityFunction);

 /**
  * Method to get minimum value of support
  */
  vnl_vector<double> GetMins() { return m_Mins; };

 /**
  * Method to set minimum value of support
  */
  void SetMins(vnl_vector<double> mins) 
  { m_Mins = mins; m_Probability = -1; };

 /**
  * Method to get maximum value of support
  */
  vnl_vector<double> GetMaxs() { return m_Maxs; };

 /**
  * Method to set maximum value of support
  */
  void SetMaxs(vnl_vector<double> maxs) 
  { m_Maxs = maxs; m_Probability = -1; };

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability.
  */
  double GetDensity(FeatureType feature)
  { 
    bool within_support = true;
    for ( int i = 0; i < Dimension; i++ )
      {
      if ( ( m_Mins[i] > feature[i] ) || ( feature[i] > m_Maxs[i] ) )
        {
        within_support = false;
        }
      }
    if ( within_support )
      {
      // if m_Probability needs to be calculated
      if (m_Probability == -1)
        {
        for ( int i = 0; i < Dimension; i++ )
          {
          if ( ( m_Mins[i] <= feature[i] ) || ( feature[i] <= m_Maxs[i] ) )
            {
            m_Probability = m_Probability*(m_Maxs[i] - m_Mins[i]);
            }
          }
        m_Probability = -1/m_Probability;
        }
      return m_Probability;      
      }
    else
      {
      return 0;
      }
  }
 

protected:		
  UniformDensityFunction(void){ m_Probability = -1;};
  ~UniformDensityFunction(void){};

  vnl_vector<double> m_Mins; // vector containing lower bound of support
  vnl_vector<double> m_Maxs; // vector containing upper bound of support
  double m_Probability;   // probability is initially set to negative value
                          // If GetProbability is called once,
                          // the probability is replaced by real value
};

} // end namespace itk



#endif

