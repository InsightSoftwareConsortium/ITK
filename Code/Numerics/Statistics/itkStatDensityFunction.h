/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatDensityFunction_h
#define __itkStatDensityFunction_h


#include <vector>


#include "itkObject.h"

namespace itk{

/** \class DensityFunction
 * \brief DensityFunction provides a base class to model
 * statistical functions. 
 */

template <unsigned int FeatureDimension, class TFeature = double>
class ITK_EXPORT DensityFunction : public Object
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef DensityFunction Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;

 /**
  * Run-time type information 
  */
  itkTypeMacro(DensityFunction,Object);

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability
  */
  virtual double GetProbability(vnl_vector<TFeature> feature) = 0;

protected:		
  DensityFunction() {};
		virtual ~DensityFunction(){};

		
};

} // end namespace itk



#endif
