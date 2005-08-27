/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFileWriter_txx
#define _itkImageFileWriter_txx

#include "itkImageFileWriter.h"
#include "itkDataObject.h"
#include "itkObjectFactoryBase.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"
#include "vnl/vnl_vector.h"
#include "itkVectorImage.h"

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
  m_UseCompression = false;
  m_UseInputMetaDataDictionary = true;
  m_FactorySpecifiedImageIO = false;
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
    m_FactorySpecifiedImageIO = true;
    }
  else
    {
    if( m_FactorySpecifiedImageIO && !m_ImageIO->CanWriteFile( m_FileName.c_str() ) )
      {
      itkDebugMacro(<<"ImageIO exists but doesn't know how to write file:" 
                    << m_FileName );
      itkDebugMacro(<<"Attempting creation of ImageIO with a factory for file:"
                    << m_FileName);
      m_ImageIO = ImageIOFactory::CreateImageIO( m_FileName.c_str(), 
                                                 ImageIOFactory::WriteMode );
      m_FactorySpecifiedImageIO = true;
      }
    }

  if ( m_ImageIO.IsNull() )
    {
    ImageFileWriterException e(__FILE__, __LINE__);
    OStringStream msg;
    msg << " Could not create IO object for file "
        << m_FileName.c_str() << std::endl;
    msg << "  Tried to create one of the following:" << std::endl;
    std::list<LightObject::Pointer> allobjects = 
      ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
    for(std::list<LightObject::Pointer>::iterator i = allobjects.begin();
        i != allobjects.end(); ++i)
      {
      ImageIOBase* io = dynamic_cast<ImageIOBase*>(i->GetPointer());
      msg << "    " << io->GetNameOfClass() << std::endl; 
      }
    msg << "  You probably failed to set a file suffix, or" << std::endl;
    msg << "    set the suffix to an unsupported type." << std::endl;
    e.SetDescription(msg.str().c_str());
    throw e;
    return;
    }

  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(input);

  typedef typename TInputImage::RegionType   RegionType;

  if ( ! m_UserSpecifiedIORegion )
    {
    // Make sure the data is up-to-date.
    if( nonConstImage->GetSource() )
      {
      nonConstImage->GetSource()->UpdateLargestPossibleRegion();
      }
    // Write the whole image
    ImageIORegion ioRegion(TInputImage::ImageDimension);
    RegionType region = input->GetLargestPossibleRegion();

    for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
      {
      ioRegion.SetSize(i,region.GetSize(i));
      ioRegion.SetIndex(i,region.GetIndex(i));
      }
    m_IORegion = ioRegion; //used by GenerateData
    }
  else
    {
    nonConstImage->Update();
    }

  // Setup the ImageIO
  //
  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  RegionType region = input->GetLargestPossibleRegion();
  const typename TInputImage::SpacingType& spacing = input->GetSpacing();
  const typename TInputImage::PointType& origin = input->GetOrigin();
  const typename TInputImage::DirectionType& direction = input->GetDirection();

  for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
    {
    m_ImageIO->SetDimensions(i,region.GetSize(i));
    m_ImageIO->SetSpacing(i,spacing[i]);
    m_ImageIO->SetOrigin(i,origin[i]);
    vnl_vector< double > axisDirection(TInputImage::ImageDimension);
// Please note: direction cosines are stored as columns of the
// direction matrix
    for(unsigned int j=0; j<TInputImage::ImageDimension; j++)
      {
      axisDirection[j] = direction[j][i];
      }
    m_ImageIO->SetDirection( i, axisDirection );
    }

  m_ImageIO->SetUseCompression(m_UseCompression);
  m_ImageIO->SetIORegion(m_IORegion);
  if( m_UseInputMetaDataDictionary )
    {
    m_ImageIO->SetMetaDataDictionary(input->GetMetaDataDictionary());
    }
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

  if( strcmp( input->GetNameOfClass(), "VectorImage" ) == 0 ) 
    {
    typedef typename InputImageType::InternalPixelType VectorImageScalarType;
    m_ImageIO->SetPixelTypeInfo( typeid(VectorImageScalarType) );

    // Since we need to set the vector length via a method available 
    // only in VectorImage, we will resort to C style UNSAFE casts !

#define ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(type) \
      else if( typeid( const VectorImage<type,3> * ) == typeid(input) ) \
      { \
      m_ImageIO->SetNumberOfComponents(((const VectorImage< type > *) \
                                   (( const void* )input))->GetVectorLength()); \
      }

    if(0)
      {
      }
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(unsigned char)
#ifdef __BORLANDC__
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( signed char)
#else
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( char)
#endif
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(unsigned short)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( short)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(unsigned int)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( int)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(unsigned long)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( long)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK(float)
    ITK_WRITER_SET_VECTOR_LENGTH_IF_BLOCK( double)
    else
      {
      OStringStream msg;
      msg <<"The VectorImage should contain blocks with InternalPixelType of: "
          << std::endl << "    " << typeid(unsigned char).name()
          << std::endl << "    " << typeid(char).name()
          << std::endl << "    " << typeid(unsigned short).name()
          << std::endl << "    " << typeid(short).name()
          << std::endl << "    " << typeid(unsigned int).name()
          << std::endl << "    " << typeid(int).name()
          << std::endl << "    " << typeid(unsigned long).name()
          << std::endl << "    " << typeid(long).name()
          << std::endl << "    " << typeid(float).name()
          << std::endl << "    " << typeid(double).name()
          << std::endl << "    "
          << m_ImageIO->GetComponentTypeAsString(m_ImageIO->GetComponentType())
          << std::endl << " is not one of them."
          << std::endl;
      itkExceptionMacro( << msg.str().c_str() );
      }
    }
  else
    {
    // Set the pixel and component type; the number of components.
    m_ImageIO->SetPixelTypeInfo(typeid(ScalarType));  
    }

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


  if (m_UseCompression)
    {
    os << indent << "Compression: On\n";
    }
  else
    {
    os << indent << "Compression: Off\n";
    }

  if (m_UseInputMetaDataDictionary)
    {
    os << indent << "UseInputMetaDataDictionary: On\n";
    }
  else
    {
    os << indent << "UseInputMetaDataDictionary: Off\n";
    }

  if (m_FactorySpecifiedImageIO)
    {
    os << indent << "FactorySpecifiedmageIO: On\n";
    }
  else
    {
    os << indent << "FactorySpecifiedmageIO: Off\n";
    }

}

} // end namespace itk

#endif
