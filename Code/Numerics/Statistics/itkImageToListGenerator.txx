/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToListGenerator_txx
#define __itkImageToListGenerator_txx

#include "itkImageToListGenerator.h"
#include "itkImageRegionConstIterator.h"

namespace itk { 
namespace Statistics {

template < class TImage, class TMaskImage >
ImageToListGenerator< TImage, TMaskImage >
::ImageToListGenerator()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);
  typename ListSampleOutputType::Pointer listSampleDecorator = 
    static_cast< ListSampleOutputType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNthOutput(0, listSampleDecorator.GetPointer());
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "MaskValue: "
     << static_cast<typename NumericTraits<MaskPixelType>::PrintType>(
       m_MaskValue)
     << std::endl;
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::SetInput(const ImageType* image) 
{ 
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< ImageType* >( image ) );
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::SetMaskImage(const MaskImageType* image) 
{ 
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, 
                                   const_cast< MaskImageType* >( image ) );
}

template < class TImage, class TMaskImage >
const TImage*
ImageToListGenerator< TImage, TMaskImage >
::GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const ImageType * >
    (this->ProcessObject::GetInput(0) );
}  

template < class TImage, class TMaskImage >
const TMaskImage*
ImageToListGenerator< TImage, TMaskImage >
::GetMaskImage() const
{
  if (this->GetNumberOfInputs() < 2)
    {
    return 0;
    }
  
  return static_cast<const MaskImageType * >
    (this->ProcessObject::GetInput(1) );
}  

template < class TImage, class TMaskImage >
typename ImageToListGenerator< TImage, TMaskImage >::DataObjectPointer
ImageToListGenerator< TImage, TMaskImage >
::MakeOutput(unsigned int itkNotUsed(idx))
{
  typename ListSampleOutputType::Pointer decoratedOutput =
    ListSampleOutputType::New();
  decoratedOutput->Set( ListSampleType::New() );
  return static_cast< DataObject * >(decoratedOutput.GetPointer());
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::GenerateData()
{
  ListSampleOutputType * decoratedOutput =
    static_cast< ListSampleOutputType * >(
      this->ProcessObject::GetOutput(0));
  ListSampleType *output = decoratedOutput->Get();
  ImageType *input = const_cast< ImageType * >(this->GetInput());
  MaskImageType *maskImage = NULL;
  
  output->Clear();

  if (this->GetNumberOfInputs() > 1)
    {
    maskImage = const_cast< MaskImageType * >(this->GetMaskImage());
    }

  typedef ImageRegionConstIterator< ImageType >     IteratorType; 
  IteratorType it( input, input->GetBufferedRegion() );
  it.GoToBegin();
  
  if (maskImage) // mask specified
    {
    typedef ImageRegionConstIterator< MaskImageType > MaskIteratorType;
    MaskIteratorType mit( maskImage, maskImage->GetBufferedRegion() );
    mit.GoToBegin();
    while (!it.IsAtEnd())
      {
      if (mit.Get() == this->m_MaskValue)
        {
        MeasurementVectorType m;
        m[0] = it.Get();
        output->PushBack(m);
        }
      ++mit;
      ++it;
      }
    }
  else // no mask specified
    {
    while (!it.IsAtEnd())
      {
      MeasurementVectorType m;
      m[0] = it.Get();
      output->PushBack(m);
      ++it;
      }
    }
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  ListSampleOutputType * decoratedOutput =
    static_cast< ListSampleOutputType * >(
      this->ProcessObject::GetOutput(0));
  ListSampleType *output = decoratedOutput->Get();
  output->SetMeasurementVectorSize( 
    itkGetStaticConstMacro( MeasurementVectorSize ));
}

template < class TImage, class TMaskImage >
void
ImageToListGenerator< TImage, TMaskImage >
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // Make sure that the mask's requested region, if specified is at least 
  // as large as the input image's buffered region. If not funny things can 
  // happen such as the mask iterator going out of bounds etc.. 
  // 
  // TODO: Why don't most other ITK filters that take multiple inputs check
  // for this ? 
  //
  if (this->GetNumberOfInputs() > 1)
    {
    MaskImageType *maskImage =
      const_cast< MaskImageType * >(this->GetMaskImage());
    ImageType     *image =
      const_cast< ImageType * >( this->GetInput() );
    if (!image->GetBufferedRegion().IsInside( maskImage->GetBufferedRegion()) )
      {
      maskImage->SetRequestedRegion( image->GetBufferedRegion() );
      }
    }
}

template < class TImage, class TMaskImage >
typename ImageToListGenerator< TImage, TMaskImage >::ListSampleType *
ImageToListGenerator< TImage, TMaskImage >
::GetListSample()
{
  ListSampleOutputType * decoratedOutput =
    static_cast< ListSampleOutputType * >(
      this->ProcessObject::GetOutput(0));
  return decoratedOutput->Get();
}

} // end of namespace Statistics 
} // end of namespace itk

#endif
