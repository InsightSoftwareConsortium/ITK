/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageSeriesWriter_txx
#define _itkImageSeriesWriter_txx

#include "itkImageSeriesWriter.h"
#include "itkDataObject.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"

namespace itk
{

//---------------------------------------------------------
template <class TInputImage>
ImageSeriesWriter<TInputImage>
::ImageSeriesWriter():
  m_FileIterator(0),
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_UserSpecifiedIORegion(false)
{
}


//---------------------------------------------------------
template <class TInputImage>
ImageSeriesWriter<TInputImage>
::~ImageSeriesWriter()
{
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageSeriesWriter<TInputImage>
::SetInput(const InputImageType *input)
{
  // ProcessObject is not const_correct so this cast is required here.
  this->ProcessObject::SetNthInput(0, 
      const_cast<TInputImage *>(input ) );
}


//---------------------------------------------------------
template <class TInputImage>
const typename ImageSeriesWriter<TInputImage>::InputImageType *
ImageSeriesWriter<TInputImage>
::GetInput(void)
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0));
}
  
//---------------------------------------------------------
template <class TInputImage>
const typename ImageSeriesWriter<TInputImage>::InputImageType *
ImageSeriesWriter<TInputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(idx));
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageSeriesWriter<TInputImage>
::SetIORegion (const ImageIORegion* region) 
{
  itkDebugMacro("setting IORegion to " << region );
  if ( m_IORegion != region)
    {
    m_IORegion = region;
    this->Modified();
    m_UserSpecifiedIORegion = true;
    }
} 

//---------------------------------------------------------
template <class TInputImage>
void 
ImageSeriesWriter<TInputImage>
::Write(void)
{
  const InputImageType * input = this->GetInput();

  itkDebugMacro( <<"Writing an image file" );

  // Make sure input is available
  if ( input == 0 )
    {
    itkExceptionMacro(<< "No input to writer!");
    }

  // Okay, set up the FileIterator and ImageIO
  //
  if ( m_FileIterator == 0 )
    {
    if ( m_ImageIO == 0 )
      {
      itkExceptionMacro(<< "Either a file iterator or ImageIO must be set");
      }
    else
      {
      m_FileIterator = m_ImageIO->NewFileIterator();
      }
    }
  else //have a FileIterator, may have to create ImageIO
    {
    const char *format = m_FileIterator->GetSeriesFormat();
    if ( m_ImageIO == 0 ) //try creating via factory
      {
      itkDebugMacro(<<"Attempting factory creation of ImageIO" << format);
      m_ImageIO = ImageIOFactory::CreateImageIO( format,
                                                 ImageIOFactory::WriteMode );
      }
    else
      {
      if( !m_ImageIO->CanWriteFile( format ) )
        {
        itkDebugMacro(<<"ImageIO exists but doesn't know how to write: "
                      << format );
        itkDebugMacro(<<"Attempting factory creation of ImageIO:" << format);
        m_ImageIO = ImageIOFactory::CreateImageIO( format,
                                                   ImageIOFactory::WriteMode );
        }
      }
    }

  if ( m_ImageIO == 0 || m_FileIterator == 0 )
    {
    itkExceptionMacro(<<"Cannot determine what type of files to create.");
    return;
    }

  // Make sure the data is up-to-date.
  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(input);
  nonConstImage->Update();

  if ( ! m_UserSpecifiedIORegion )
    {
    // Write the whole image
    ImageIORegion ioRegion(TInputImage::ImageDimension);
    ImageRegion<TInputImage::ImageDimension> region = 
          input->GetLargestPossibleRegion();

    for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
      {
      ioRegion.SetSize(i,region.GetSize(i));
      ioRegion.SetIndex(i,region.GetIndex(i));
      }
    m_IORegion = ioRegion;
    }

  // Setup the ImageIO
  //
  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  ImageRegion<TInputImage::ImageDimension> region = 
        input->GetLargestPossibleRegion();
  const double *spacing = input->GetSpacing();
  const double *origin = input->GetOrigin();

  for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
    {
    m_ImageIO->SetDimensions(i,region.GetSize(i));
    m_ImageIO->SetSpacing(i,spacing[i]);
    m_ImageIO->SetOrigin(i,origin[i]);
    }
  m_ImageIO->SetIORegion(m_IORegion);

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Actually do something
  this->GenerateData();
  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  if ( input->ShouldIReleaseData() )
    {
    nonConstImage->ReleaseData();
    }
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageSeriesWriter<TInputImage>
::GenerateData(void)
{
  const InputImageType * input = this->GetInput();

  itkDebugMacro(<<"Writing a series of files");
  
  // The dimension of the input will likely not match the supported
  // dimensionality of the file format (why else use the image series
  // writer?) Files are written in the supported dimension; the total
  // number of files written are dims[M]*dims[M-1]...dims[M-N] where M
  // is the dimension of the imput image, and N is the dimension of the
  // supported file format (note M > N and dims[i] is the dimension of
  // the image in the ith direction).
  const unsigned long imageDimension = TInputImage::ImageDimension;
  unsigned long supportedDimension = imageDimension;
  while ( supportedDimension > 0 &&
          ! m_ImageIO->SupportsDimension(supportedDimension) )
    {
    supportedDimension--;
    }
  if ( supportedDimension <= 0 )
    {
    itkExceptionMacro(<< "File format does not support series!");
    }

  // Okay, the number of slices to write is computed here. Also create
  // and begin setting up the image IO object. Note: the lower dimensions 
  // of the IO region do not change, it is the upper dimensions that 
  // change as slices are written.
  //
  unsigned long i = imageDimension;
  unsigned long numberOfSlices=1;
  ImageIORegion ioRegion(TInputImage::ImageDimension);
  for ( i=0; i < TInputImage::ImageDimension; i++ )
    {
    if ( i >= supportedDimension )
      {
      numberOfSlices *= m_IORegion.GetSize(i);
      ioRegion.SetSize(i,1);
      }
    else
      {
      ioRegion.SetSize(i,m_IORegion.GetSize(i));
      }
    }

  // Make sure that the image is the right type and no more than 
  // four components.
  typedef typename InputImageType::PixelType ScalarType;

  // Set the pixel and component type; the number of components.
  const int ret =  m_ImageIO->SetPixelType(typeid(ScalarType));  
  itkDebugMacro(<<" PixelType is supported: " << ret );

  // Get the pointer to the image
  const void* dataPtr = (const void*) input->GetBufferPointer();
  
  // Loop over the image adjusting the IO Region as appropriate
  //
  m_FileIterator->Begin();
  for (int slice=0; slice < m_IORegion.GetSize(2); slice++)
    {
    m_ImageIO->SetFileName((*(*m_FileIterator)).c_str());
    ioRegion.SetIndex(2,slice);
    m_ImageIO->SetIORegion(ioRegion);
    m_ImageIO->Write(dataPtr);
    ++(*m_FileIterator);
    }

  itkDebugMacro(<<"Wrote " << numberOfSlices << " slices.");
}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageSeriesWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Image IO: ";
  if ( m_ImageIO == 0 )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_ImageIO << "\n";
    }
  
  os << indent << "File Iterator: ";
  if ( m_FileIterator == 0 )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_FileIterator << "\n";
    }
}

} // end namespace itk

#endif
