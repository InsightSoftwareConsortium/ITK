  /*=========================================================================

    Program:   Insight Segmentation & Registration Toolkit
    Module:    itkMeanImageFunction.txx
    Language:  C++
    Date:      $Date$
    Version:   $Revision$

    Copyright (c) 2002 Insight Consortium. All rights reserved.
    See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

       This software is distributed WITHOUT ANY WARRANTY; without even 
       the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
       PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
  #ifndef _itkMeanImageFunction_txx
  #define _itkMeanImageFunction_txx
  #include "itkMeanImageFunction.h"

  #include "itkNumericTraits.h"
  #include "itkConstSmartNeighborhoodIterator.h"

  namespace itk
  {

  /**
   * Constructor
   */
  template <class TInputImage, class TCoordRep>
  MeanImageFunction<TInputImage,TCoordRep>
  ::MeanImageFunction()
  {
  m_NeighborhoodRadius = 1;
  }


  /**
   *
   */
  template <class TInputImage, class TCoordRep>
  void
MeanImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
typename MeanImageFunction<TInputImage,TCoordRep>
::RealType
MeanImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(const IndexType& index) const
{
  RealType sum;

  sum = NumericTraits<RealType>::Zero;
  
  if( !m_Image )
    {
    return ( NumericTraits<RealType>::max() );
    }
  
  if ( !this->IsInsideBuffer( index ) )
    {
    return ( NumericTraits<RealType>::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill( m_NeighborhoodRadius );
  
  ConstSmartNeighborhoodIterator<InputImageType>
    it(kernelSize, m_Image, m_Image->GetBufferedRegion());

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for (unsigned int i = 0; i < size; ++i)
    {
    sum += static_cast<RealType>(it.GetPixel(i));
    }
  sum /= double(it.Size());
             
  return ( sum );
}


} // namespace itk

#endif
