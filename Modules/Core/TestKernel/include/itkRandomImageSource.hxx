/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkRandomImageSource_hxx
#define itkRandomImageSource_hxx

#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 *
 */
template< typename TOutputImage >
RandomImageSource< TOutputImage >
::RandomImageSource()
{
  //Initial image is 64 wide in each direction.
  for ( unsigned int i = 0; i < TOutputImage::GetImageDimension(); i++ )
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
  m_Direction.SetIdentity();

  m_Min = NumericTraits< OutputImagePixelType >::NonpositiveMin();
  m_Max = NumericTraits< OutputImagePixelType >::max();
}

template< typename TOutputImage >
RandomImageSource< TOutputImage >
::~RandomImageSource()
{}

template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::SetSize(SizeValueArrayType sizeArray)
{
  const unsigned int count = TOutputImage::ImageDimension;
  unsigned int       i;

  for ( i = 0; i < count; i++ )
    {
    if ( sizeArray[i] != this->m_Size[i] )
      {
      break;
      }
    }
  if ( i < count )
    {
    this->Modified();
    for ( i = 0; i < count; i++ )
      {
      this->m_Size[i] = sizeArray[i];
      }
    }
}

template< typename TOutputImage >
const typename RandomImageSource< TOutputImage >::SizeValueType *
RandomImageSource< TOutputImage >
::GetSize() const
{
  return this->m_Size.GetSize();
}

template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::SetSpacing(SpacingValueArrayType spacingArray)
{
  const unsigned int count = TOutputImage::ImageDimension;
  unsigned int       i;

  for ( i = 0; i < count; i++ )
    {
    if ( Math::NotExactlyEquals(spacingArray[i], this->m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < count )
    {
    this->Modified();
    for ( i = 0; i < count; i++ )
      {
      this->m_Spacing[i] = spacingArray[i];
      }
    }
}

template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::SetOrigin(PointValueArrayType originArray)
{
  const unsigned int count = TOutputImage::ImageDimension;
  unsigned int       i;

  for ( i = 0; i < count; i++ )
    {
    if ( Math::NotExactlyEquals(originArray[i], this->m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < count )
    {
    this->Modified();
    for ( i = 0; i < count; i++ )
      {
      this->m_Origin[i] = originArray[i];
      }
    }
}

template< typename TOutputImage >
const typename RandomImageSource< TOutputImage >::PointValueType *
RandomImageSource< TOutputImage >
::GetOrigin() const
{
  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    this->m_OriginArray[i] = this->m_Origin[i];
    }
  return this->m_OriginArray;
}

template< typename TOutputImage >
const typename RandomImageSource< TOutputImage >::SpacingValueType *
RandomImageSource< TOutputImage >
::GetSpacing() const
{
  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    this->m_SpacingArray[i] = this->m_Spacing[i];
    }
  return this->m_SpacingArray;
}

/**
 *
 */
template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Max: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_Max )
     << std::endl;
  os << indent << "Min: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_Min )
     << std::endl;

  os << indent << "Origin: [";
  unsigned int ii = 0;
  while( ii < TOutputImage::ImageDimension - 1 )
    {
    os << m_Origin[ii] << ", ";
    ++ii;
    }
  os << m_Origin[ii] << "]" << std::endl;

  os << indent << "Spacing: [";
  ii = 0;
  while( ii < TOutputImage::ImageDimension - 1 )
    {
    os << m_Spacing[ii] << ", ";
    ++ii;
    }
  os << m_Spacing[ii] << "]" << std::endl;

  os << indent << "Size: [";
  ii = 0;
  while( ii < TOutputImage::ImageDimension - 1 )
    {
    os << m_Size[ii] << ", ";
    ++ii;
    }
  os << m_Size[ii] << "]" << std::endl;
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::GenerateOutputInformation()
{
  TOutputImage *output;
  IndexType     index;

  index.Fill(0);

  output = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(this->m_Size);
  largestPossibleRegion.SetIndex(index);
  output->SetLargestPossibleRegion(largestPossibleRegion);

  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
  output->SetDirection(m_Direction);
}

//----------------------------------------------------------------------------
template< typename TOutputImage >
void
RandomImageSource< TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  itkDebugMacro(<< "Generating a random image of scalars");

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  typedef typename TOutputImage::PixelType scalarType;
  typename TOutputImage::Pointer image = this->GetOutput(0);

  ImageRegionIterator< TOutputImage > it(image, outputRegionForThread);

  // Random number seed
  unsigned int sample_seed = 12345 + threadId;
  double       u;
  double       rnd;

  double dMin = static_cast< double >( m_Min );
  double dMax = static_cast< double >( m_Max );

  for (; !it.IsAtEnd(); ++it )
    {
    sample_seed = ( sample_seed * 16807 ) % 2147483647L;
    u = static_cast< double >( sample_seed ) / 2147483711UL;
    rnd = ( 1.0 - u ) * dMin + u * dMax;

    it.Set( (scalarType)rnd );
    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif
