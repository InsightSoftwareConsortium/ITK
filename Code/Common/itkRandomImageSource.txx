/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRandomImageSource.h"
#include "itkPixelTraits.h"
#include "itkObjectFactory.h"
#include "itkImageScalarRegionIterator.h"
#include "vnl/vnl_sample.h"

 
namespace itk
{

/**
 *
 */
template <class TOutputImage>
RandomImageSource<TOutputImage>
::RandomImageSource()
{
  m_Size = new unsigned long [TOutputImage::GetImageDimension()];
  m_Spacing = new float [TOutputImage::GetImageDimension()];
  m_Origin = new float [TOutputImage::GetImageDimension()];  

  //Initial image is 64 wide in each direction.
  for (int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }

  m_Min = NumericTraits<OutputImageScalarValueType>::min();
  m_Max = NumericTraits<OutputImageScalarValueType>::max();

}

template <class TOutputImage>
RandomImageSource<TOutputImage>
::~RandomImageSource()
{
  delete [] m_Size;
  delete [] m_Spacing;
  delete [] m_Origin;
}

/**
 *
 */
template <class TOutputImage>
void 
RandomImageSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}


/**
 * Microsoft compiler defines these and screws up the traits
 */

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::IndexType index = {0};
  typename TOutputImage::SizeType size = {0};
  size.SetSize( m_Size );
  
  output = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize( size );
  largestPossibleRegion.SetIndex( index );
  output->SetLargestPossibleRegion( largestPossibleRegion );

  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::ThreadedGenerateData(const OutputImageRegion& outputRegionForThread,
                       int threadId )
{
  itkDebugMacro(<<"Generating a random image of scalars");

  typedef typename TOutputImage::ScalarValueType scalarType;
  typename TOutputImage::Pointer image=this->GetOutput(0);
  image->SetBufferedRegion( image->GetRequestedRegion() );

  ImageScalarRegionIterator<OutputImagePixelType, TOutputImage::ImageDimension>
    scalarIterator(image, outputRegionForThread);

  for ( ; !scalarIterator.IsAtEnd(); ++scalarIterator)
    {
    *scalarIterator = (scalarType) vnl_sample_uniform(
                                   (double)m_Min,(double)m_Max);
    }
}

} // end namespace itk
