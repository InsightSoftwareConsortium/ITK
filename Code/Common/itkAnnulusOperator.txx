/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnnulusOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnnulusOperator_txx
#define __itkAnnulusOperator_txx

#include "itkAnnulusOperator.h"
#include "itkSphereSpatialFunction.h"

namespace itk
{

/** Create the operator */
template <class TPixel, unsigned int TDimension, class TAllocator>
void
AnnulusOperator<TPixel, TDimension, TAllocator>
::CreateOperator()
{
  CoefficientVector coefficients;
  
  coefficients = this->GenerateCoefficients();

  this->Fill(coefficients);
}

/** This function fills the coefficients into the corresponding 
 *  neighborhood. */
template <class TPixel, unsigned int TDimension, class TAllocator>
void  
AnnulusOperator <TPixel, TDimension, TAllocator>
::Fill(const CoefficientVector &coeff)
{
  typename Superclass::CoefficientVector::const_iterator it;

  std::slice* temp_slice;
  temp_slice = new std::slice(0, coeff.size(),1);
  
  typename Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;
 
  it = coeff.begin();

  // Copy the coefficients into the neighborhood
  for (data = data.Begin(); data < data.End(); ++data, ++it)
    {
    *data = *it;
    }
}

template <class TPixel, unsigned int TDimension, class TAllocator>
typename AnnulusOperator<TPixel, TDimension, TAllocator>
::CoefficientVector
AnnulusOperator<TPixel, TDimension, TAllocator>
::GenerateCoefficients()
{
  // Determine the initial kernel values...
  double interiorV, annulusV, exteriorV;
  if (m_Normalize)
    {
    double bright = (m_BrightCenter ? 1.0 : -1.0);
    
    // Initial values for a normalized kernel
    interiorV = bright;
    annulusV = -1.0*bright;
    exteriorV = 0.0;
    }
  else
    {
    // values for a specified kernel
    interiorV = m_InteriorValue;
    annulusV = m_AnnulusValue;
    exteriorV = m_ExteriorValue;
    }

  // Compute the size of the kernel in pixels
  typedef typename SizeType::SizeValueType SizeValueType;
  SizeType r;
  unsigned int i, j;
  double outerRadius = m_InnerRadius + m_Thickness;
  for (i=0; i < TDimension; ++i)
    {
    r[i] = static_cast<SizeValueType>(ceil(outerRadius / m_Spacing[i]));
    }
  this->SetRadius(r);

  // Use a couple of sphere spatial functions...
  typedef SphereSpatialFunction<TDimension> SphereType;
  typename SphereType::Pointer innerS = SphereType::New();
  typename SphereType::Pointer  outerS = SphereType::New();
  
  innerS->SetRadius( m_InnerRadius );
  outerS->SetRadius( m_InnerRadius + m_Thickness );
  
  // Walk the neighborhood (this) and evaluate the sphere spatial
  // functions
  bool inInner, inOuter;
  double sumNotExterior = 0.0;
  double sumNotExteriorSq = 0.0;
  unsigned int countNotExterior = 0;
  
  unsigned int w;
  w = this->Size();

  std::vector<bool> outside(w);
  CoefficientVector coeffP(w);
  OffsetType offset;
  typename SphereType::InputType point;

  for (i=0; i < w; ++i)
    {
    // get the offset from the center pixel
    offset = this->GetOffset(i);

    // convert to a position
    for (j=0; j < TDimension; ++j)
      {
      point[j] = m_Spacing[j] * offset[j];
      }

    // evaluate the spheres
    inInner = innerS->Evaluate(point);
    inOuter = outerS->Evaluate(point);

    // set the coefficients
    if (!inOuter)
      {
      // outside annulus
      coeffP[i] = exteriorV;
      outside[i] = true;
      }
    else if (!inInner)
      {
      // inside the outer circle but outside the inner circle
      coeffP[i] = annulusV;
      sumNotExterior += annulusV;
      sumNotExteriorSq += (annulusV*annulusV);
      countNotExterior++;
      outside[i] = false;
      }
    else
      {
      // inside inner circle
      coeffP[i] = interiorV;
      sumNotExterior += interiorV;
      sumNotExteriorSq += (interiorV*interiorV);
      countNotExterior++;
      outside[i] = false;
      }
    }

  // Normalize the kernel if necessary
  if (m_Normalize)
    {
    // Calculate the mean and standard deviation of kernel values NOT
    // the exterior
    double num = static_cast<double>(countNotExterior);
    double mean = sumNotExterior / num;
    double var = ( sumNotExteriorSq - ( sumNotExterior*sumNotExterior / num ) )
      / ( num - 1.0 );
    double std = sqrt(var);

    // convert std to a scaling factor k such that
    //
    //        || (coeffP - mean) / k || = 1.0
    //
    double k = std * sqrt(num-1.0);

    // Run through the kernel again, shifting and normalizing the
    // elements that are not exterior to the annulus.  This forces the
    // kernel to have mean zero and norm 1 AND forces the region
    // outside the annulus to have no influence.
    for (i=0; i < w; ++i)
      {
      // normalize the coefficient if it is inside the outer circle
      // (exterior to outer circle is already zero)
      if (!outside[i])
        {
        coeffP[i] = (coeffP[i] - mean) / k;
        }
      }
    }
 

  return coeffP;
}

} // namespace itk

#endif
