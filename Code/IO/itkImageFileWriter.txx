/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFileWriter_txx
#define _itkImageFileWriter_txx

#include "itkImageFileWriter.h"
#include "itkDataObject.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"

namespace itk
{

//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>
::ImageFileWriter():
  m_FileName(""),m_FilePrefix(""),m_FilePattern(""),
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_Region(0), m_UserSpecifiedRegion(false)
{
}


//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>
::~ImageFileWriter()
{
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::SetInput(const InputImageType *input)
{
  // ProcessObject is not const_correct so this cast is required here.
  this->ProcessObject::SetNthInput(0, 
      const_cast<TInputImage *>(input ) );
}


//---------------------------------------------------------
template <class TInputImage>
const typename ImageFileWriter<TInputImage>::InputImageType *
ImageFileWriter<TInputImage>
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
const typename ImageFileWriter<TInputImage>::InputImageType *
ImageFileWriter<TInputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(idx));
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::Write(void)
{
  const InputImageType * input = this->GetInput();
  if ( input == 0 )
    {
    itkExceptionMacro(<<"No input to writer!");
    return;
    }

  
  // This is required due to the lack of const-correctness of 
  // the ProcessObject
  InputImageType * nonConstInput = const_cast<InputImageType*>( input );
  nonConstInput->Update();

  if ( m_ImageIO.IsNull() ) //try creating via factory
    {
    itkDebugMacro(<<"Attempting creation of ImageIO with a factory for file " << m_FileName);
    m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), ImageIOFactory::WriteMode );
    }
  else
    {
      if( !m_ImageIO->CanWriteFile( m_FileName.c_str() ) )
        {
        itkDebugMacro(<<"ImageIO exists but doesn't know how to write file" << m_FileName );
        itkDebugMacro(<<"Attempting creation of ImageIO with a factory for file " << m_FileName);
        m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), ImageIOFactory::WriteMode );
        }
    }

  if ( m_ImageIO.IsNull() )
    {
    itkExceptionMacro(<<"No ImageIO set, or none could be created.");
    return;
    }

  // Make sure region is within the image, crop if necessary
  ImageIORegion ioRegion(TInputImage::ImageDimension);
  ImageRegion<TInputImage::ImageDimension> region = 
        input->GetLargestPossibleRegion();
  const double *spacing = input->GetSpacing();
  const double *origin = input->GetOrigin();

  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
    {
    ioRegion.SetSize(i,region.GetSize(i));
    ioRegion.SetIndex(i,region.GetIndex(i));
    m_ImageIO->SetDimensions(i,region.GetSize(i));
    m_ImageIO->SetSpacing(i,spacing[i]);
    m_ImageIO->SetOrigin(i,origin[i]);
    }
  itkDebugMacro( <<"Region to write = " << ioRegion );
  this->Write(ioRegion);        
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::Write(const ImageIORegion &ioRegion)
{
  const InputImageType * input = this->GetInput();

  // make sure input is available
  if ( input == 0 )
    {
    itkExceptionMacro(<< "No input to writer!");
    return;
    }

  if ( m_ImageIO.IsNull() )
    {
    itkExceptionMacro(<<"No ImageIO set, or none could be created.");
    return;
    }

  // Check to see if we can write the file given the name or prefix
  //
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    itkExceptionMacro(<<"No filename or file prefix specified");
    return;
    }

  m_ImageIO->SetIORegion(ioRegion);

  // make sure the data is up-to-date
  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(input);
  nonConstImage->Update();

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
ImageFileWriter<TInputImage>
::GenerateOutputInformation(void)
{

  itkDebugMacro(<<"Entering GenerateOutputInformation()" << m_FileName);

  // Check to see if we can write the file given the name or prefix
  //
  if ( m_FileName == "" && m_FilePrefix == "" )
    {
    throw ImageFileWriterException(__FILE__, __LINE__, "One of FileName or FilePrefix must be non-empty");
    }

  if ( m_ImageIO.IsNull() ) //try creating via factory
    {
    m_UserSpecifiedImageIO = false;
    m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), ImageIOFactory::WriteMode );
    }
  else
    {
    m_UserSpecifiedImageIO = true;
    }
  
  if ( m_ImageIO.IsNull() )
    {
    ImageFileWriterException e(__FILE__, __LINE__);
    OStringStream msg;
    msg << " Could not create IO object for file "
        << m_FileName.c_str();
    e.SetDescription(msg.str().c_str());
    throw e;
    return;
    }


}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::GenerateData(void)
{
  const InputImageType * input = this->GetInput();

  itkDebugMacro(<<"Writing file: " << m_FileName);
  
  // Make sure that the image is the right type and no more than 
  // four components.
  typedef typename InputImageType::PixelType ScalarType;

  // Set the pixel and component type; the number of components.
  const int ret =  m_ImageIO->SetPixelType(typeid(ScalarType));  
  itkDebugMacro(<<" PixelType is supported: " << ret );

  // Setup the image IO for writing.
  //
  m_ImageIO->SetFileName(m_FileName.c_str());
  m_ImageIO->SetFilePrefix(m_FilePrefix.c_str());

  //okay, now extract the data as a raw buffer pointer
  const void* dataPtr = (const void*) input->GetBufferPointer();
  m_ImageIO->Write(dataPtr);

}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName.data() ? m_FileName.data() : "(none)") << std::endl;
  os << indent << "File Prefix: " 
     << (m_FilePrefix.data() ? m_FilePrefix.data() : "(none)") << std::endl;
  os << indent << "File Pattern: " 
     << (m_FilePattern.data() ? m_FilePattern.data() : "(none)") << std::endl;

  os << indent << "Image IO: ";
  if ( m_ImageIO.IsNull() )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_ImageIO << "\n";
    }
  
}

} // end namespace itk

#endif
