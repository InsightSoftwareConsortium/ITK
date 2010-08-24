/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListSampleFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToListSampleFilter_txx
#define __itkImageToListSampleFilter_txx

#include "itkImageToListSampleFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
namespace Statistics
{
template< class TImage, class TMaskImage >
ImageToListSampleFilter< TImage, TMaskImage >
::ImageToListSampleFilter()
{
  this->m_MaskValue = itk::NumericTraits< MaskPixelType >::max();
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaskValue: "
     << static_cast< typename NumericTraits< MaskPixelType >::PrintType >(
    this->GetMaskValue() )
     << std::endl;
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::SetInput(const ImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< ImageType * >( image ) );
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::SetMaskImage(const MaskImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< MaskImageType * >( image ) );
}

template< class TImage, class TMaskImage >
const TImage *
ImageToListSampleFilter< TImage, TMaskImage >
::GetInput() const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const ImageType * >
         ( this->ProcessObject::GetInput(0) );
}

template< class TImage, class TMaskImage >
const TMaskImage *
ImageToListSampleFilter< TImage, TMaskImage >
::GetMaskImage() const
{
  if ( this->GetNumberOfInputs() < 2 )
    {
    return 0;
    }

  return static_cast< const MaskImageType * >
         ( this->ProcessObject::GetInput(1) );
}

template< class TImage, class TMaskImage >
typename ImageToListSampleFilter< TImage, TMaskImage >::DataObjectPointer
ImageToListSampleFilter< TImage, TMaskImage >
::MakeOutput( unsigned int itkNotUsed(idx) )
{
  typename ListSampleType::Pointer output = ListSampleType::New();
  return static_cast< DataObject * >( output );
}

template< class TImage, class TMaskImage >
unsigned int
ImageToListSampleFilter< TImage, TMaskImage >
::GetMeasurementVectorSize() const
{
  const ImageType *input = this->GetInput();

  if ( input == NULL )
    {
    itkExceptionMacro("Input image has not been set yet");
    }

  MeasurementVectorType m;
  unsigned int          measurementVectorSize;

  if ( !MeasurementVectorTraits::IsResizable(m) )
    {
    measurementVectorSize = MeasurementVectorTraits::GetLength(m);
    }
  else
    {
    measurementVectorSize = input->GetNumberOfComponentsPerPixel();
    }

  return measurementVectorSize;
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::GenerateData()
{
  ListSampleType *output =
    static_cast< ListSampleType * >( this->ProcessObject::GetOutput(0) );

  const ImageType *    input = this->GetInput();
  const MaskImageType *maskImage = NULL;

  // Verify whether the image and the mask have the same LargestPossibleRegion.
  // Otherwise, throw an exception.
  //
  if ( this->GetNumberOfInputs() > 1 )
    {
    maskImage = this->GetMaskImage();

    if ( input->GetLargestPossibleRegion() != maskImage->GetLargestPossibleRegion() )
      {
      itkExceptionMacro("LargestPossibleRegion of the mask does not match the one for the image");
      }
    }

  output->Clear();

  typedef ImageRegionConstIterator< ImageType > IteratorType;
  IteratorType it( input, input->GetBufferedRegion() );
  it.GoToBegin();

  if ( maskImage ) // mask specified
    {
    typedef ImageRegionConstIterator< MaskImageType > MaskIteratorType;
    MaskIteratorType mit( maskImage, maskImage->GetBufferedRegion() );
    mit.GoToBegin();
    while ( !it.IsAtEnd() )
      {
      if ( mit.Get() == this->m_MaskValue )
        {
        MeasurementVectorType m;
        MeasurementVectorTraits::Assign( m, it.Get() );
        output->PushBack(m);
        }
      ++mit;
      ++it;
      }
    }
  else // no mask specified
    {
    while ( !it.IsAtEnd() )
      {
      MeasurementVectorType m;
      MeasurementVectorTraits::Assign( m, it.Get() );
      output->PushBack(m);
      ++it;
      }
    }
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  ListSampleType *output =
    static_cast< ListSampleType * >( this->ProcessObject::GetOutput(0) );
  output->SetMeasurementVectorSize( this->GetMeasurementVectorSize() );
}

template< class TImage, class TMaskImage >
void
ImageToListSampleFilter< TImage, TMaskImage >
::GenerateInputRequestedRegion()
throw( InvalidRequestedRegionError )
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
}

template< class TImage, class TMaskImage >
const typename ImageToListSampleFilter< TImage, TMaskImage >::ListSampleType *
ImageToListSampleFilter< TImage, TMaskImage >
::GetOutput() const
{
  const ListSampleType *output =
    static_cast< const ListSampleType * >( this->ProcessObject::GetOutput(0) );

  return output;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
