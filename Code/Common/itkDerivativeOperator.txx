/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk
{

template <class TPixel, unsigned int VDimension>
typename DerivativeOperator<TPixel, VDimension>::CoefficientVector
DerivativeOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  unsigned int i;
  unsigned int j;
  ScalarValueType previous;
  ScalarValueType next;
  const unsigned int w = 2*((m_Order + 1)/2) + 1;
  std::vector<ScalarValueType> coeff(w);
  CoefficientVector coeffP(w);

  coeff[w/2] = 1.0;
   for (i = 0; i < m_Order/2; i++)
      {
        previous = coeff[1] - 2 * coeff[0];
        for (j = 1; j < w - 1; j++)
          {
            next =coeff[j - 1]  + coeff[j + 1] - 2*coeff[j];
            coeff[j-1] = previous;
            previous = next;
          }
        next = coeff[j - 1] - 2*coeff[j];
        coeff[j-1] = previous;
        coeff[j] = next;	    
      }
    for (i = 0; i < m_Order%2; i++)    
      {
        previous =  0.5 * coeff[1];
        for (j = 1; j < w - 1; j++)
          {
            next = -0.5*coeff[j - 1] + 0.5*coeff[j + 1];
            coeff[j-1] = previous;
            previous = next;
          }
        next = -0.5 * coeff[j - 1];
        coeff[j-1] = previous;
        coeff[j] = next;	    
      }

    for (i=0; i<w; ++i)
      {
        coeffP[i] = coeff[i];
      }
    return coeffP;
    
}

} // namespace itk
