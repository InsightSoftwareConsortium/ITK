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
std::vector<TPixel>
DerivativeOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  // This routine only fills in the scalar portion of the pixels. This works
  // for all native data types and Scalar<>'s.  A different operator is
  // probably required for derivatives of vector and composite types.
  int i;
  int j;
  int h;
  TPixelScalarValueType previous;
  TPixelScalarValueType next;
  const int w = 2*((m_Order + 1)/2) + 1;
  std::vector<TPixelScalarValueType> coeff(w);
  std::vector<TPixel> coeffP(w);

  h = 1;
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

    // Copy scalar values into a TPixel array, a useless exercise for native
    // types but necessary to support itk::Scalar<>.
    for (i=0; i<w; ++i)
      {
        ScalarTraits<TPixel>::SetScalar(coeffP[i], coeff[i]);
      }
    return coeffP;
    
}

} // namespace itk
