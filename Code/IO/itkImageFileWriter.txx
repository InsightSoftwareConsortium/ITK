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
  m_FileName(""),
  m_ImageIO(0), m_UserSpecifiedImageIO(false),
  m_UserSpecifiedIORegion(false)
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
::SetIORegion (const ImageIORegion& region) 
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
ImageFileWriter<TInputImage>
::Write()
{
  const InputImageType * input = this->GetInput();

  itkDebugMacro( <<"Writing an image file" );

  // Make sure input is available
  if ( input == 0 )
    {
    itkExceptionMacro(<< "No input to writer!");
    }

  // Make sure that we can write the file given the name
  //
  if ( m_FileName == "" )
    {
    itkExceptionMacro(<<"No filename was specified");
    }

  if ( m_ImageIO.IsNull() ) //try creating via factory
    {
    itkDebugMacro(<<"Attempting factory creation of ImageIO for file: " 
                  << m_FileName);
    m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), 
                                               ImageIOFactory::WriteMode );
    }
  else
    {
    if( !m_ImageIO->CanWriteFile( m_FileName.c_str() ) )
      {
      itkDebugMacro(<<"ImageIO exists but doesn't know how to write file:" 
                    << m_FileName );
      itkDebugMacro(<<"Attempting creation of ImageIO with a factory for file:"
                    << m_FileName);
      m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), 
                                                 ImageIOFactory::WriteMode );
      }
    }

  if ( m_ImageIO.IsNull() )
    {
    itkExceptionMacro(<<"No ImageIO set, or none could be created.");
    }

  // Make sure the data is up-to-date.
  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(input);
  nonConstImage->Update();

  typedef typename TInputImage::RegionType   RegionType;

  if ( ! m_UserSpecifiedIORegion )
    {
    // Write the whole image
    ImageIORegion ioRegion(TInputImage::ImageDimension);
    RegionType region = input->GetLargestPossibleRegion();
//    const double *spacing = input->GetSpacing();
//    const double *origin = input->GetOrigin();

    for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
      {
      ioRegion.SetSize(i,region.GetSize(i));
      ioRegion.SetIndex(i,region.GetIndex(i));
      }
    m_IORegion = ioRegion; //used by GenerateData
    }

  // Setup the ImageIO
  //
  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  RegionType region = input->GetLargestPossibleRegion();
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

  os << indent << "Image IO: ";
  if ( m_ImageIO.IsNull() )
    {
    os << "(none)\n";
    }
  else
    {
    os << m_ImageIO << "\n";
    }

  os << indent << "IO Region: " << m_IORegion << "\n";
}

} // end namespace itk

#endif
