/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatUniformDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatUniformDensityFunction_h
#define __itkStatUniformDensityFunction_h

#include <vnl/vnl_vector.h>

#include "itkSmartPointer.h"
#include "itkStatDensityFunction.h"


namespace itk{

/** \class UniformDensityFunction
 * \brief This class expresses uniform density distribution 
 *
 * This class keeps parameter to define Uniform Density Function  and has
 * method to return the probability (the value of Uniform Density Function) 
 * of an instance.  FeatureDimension is the dimension of feature space.
 * TFeature is type of feature. (e.g. For RGB data, TFeature can be
 * 3-dimensional vector.  For black and white image, TFeture can be scalar)
 */

template <unsigned int FeatureDimension, class TFeature = double>
class ITK_EXPORT UniformDensityFunction 
: public DensityFunction<FeatureDimension, TFeature>
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

 /**
  * Standard Superclass typedef
  */
  typedef DensityFunction<FeatureDimension, TFeature> Superclass;

 /**
  * Dimension of the feature vector
  */
  enum { Dimension = FeatureDimension };

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
  vnl_vector<TFeature> GetMins() { return m_Mins; };

 /**
  * Method to set minimum value of support
  */
  void SetMins(vnl_vector<TFeature> mins) 
  { m_Mins = mins; m_Probability = -1; };

 /**
  * Method to get maximum value of support
  */
  vnl_vector<TFeature> GetMaxs() { return m_Maxs; };

 /**
  * Method to set maximum value of support
  */
  void SetMaxs(vnl_vector<TFeature> maxs) 
  { m_Maxs = maxs; m_Probability = -1; };

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability.
  */
  double GetProbability(vnl_vector<TFeature> feature)
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

  vnl_vector<TFeature> m_Mins; // vector containing lower bound of support
  vnl_vector<TFeature> m_Maxs; // vector containing upper bound of support
  double m_Probability;   // probability is initially set to negative value
                          // If GetProbability is called once,
                          // the probability is replaced by real value
};

} // end namespace itk



#endif
