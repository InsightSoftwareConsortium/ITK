/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRandomImageSource_txx
#define _itkRandomImageSource_txx

#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
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
  for (unsigned int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }

  m_Min = NumericTraits<OutputImagePixelType>::NonpositiveMin();
  m_Max = NumericTraits<OutputImagePixelType>::max();
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
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Max: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_Max)
     << std::endl;
  os << indent << "Min: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_Min)
     << std::endl;
  unsigned int i;
  os << indent << "Origin: [";
  for (i=0; i < TOutputImage::ImageDimension - 1; i++)
    {
    os << m_Origin[i] << ", ";
    }
  os << m_Origin[i] << "]" << std::endl;

  os << indent << "Spacing: [";
  for (i=0; i < TOutputImage::ImageDimension - 1; i++)
    {
    os << m_Spacing[i] << ", ";
    }
  os << m_Spacing[i] << "]" << std::endl;

  os << indent << "Size: [";
  for (i=0; i < TOutputImage::ImageDimension - 1; i++)
    {
    os << m_Size[i] << ", ";
    }
  os << m_Size[i] << "]" << std::endl;
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::IndexType index = {{0}};
  typename TOutputImage::SizeType size = {{0}};
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
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId )
{
  itkDebugMacro(<<"Generating a random image of scalars");

  // Support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
       
  typedef typename TOutputImage::PixelType scalarType;
  typename TOutputImage::Pointer image=this->GetOutput(0);

  ImageRegionIterator<TOutputImage> it(image, outputRegionForThread);

  // Random number seed
  unsigned long sample_seed = 12345 + threadId;
  double u;
  double rnd;

  double dMin = static_cast<double>(m_Min);
  double dMax = static_cast<double>(m_Max);

  for ( ; !it.IsAtEnd(); ++it)
    {

    sample_seed = (sample_seed*16807)%2147483647L;
    u = static_cast<double>(sample_seed)/2147483711UL;
    rnd = (1.0 - u)*dMin + u*dMax;

    it.Set( (scalarType) rnd);
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
