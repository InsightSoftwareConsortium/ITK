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
#ifndef __itkImageFileWriter_txx
#define __itkImageFileWriter_txx

#include "itkImageFileWriter.h"
#include "itkDataObject.h"
#include "itkObjectFactoryBase.h"
#include "itkImageIOFactory.h"
#include "itkCommand.h"
#include "vnl/vnl_vector.h"
#include "itkVectorImage.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

//---------------------------------------------------------
template <class TInputImage>
ImageFileWriter<TInputImage>
::ImageFileWriter():
  m_PasteIORegion(TInputImage::ImageDimension)
{
  m_UseCompression = false;
  m_UseInputMetaDataDictionary = true;
  m_FactorySpecifiedImageIO = false;
  m_UserSpecifiedIORegion = false;
  m_UserSpecifiedImageIO = false;
  m_NumberOfStreamDivisions = 1;
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
  return static_cast<TInputImage*> (this->ProcessObject::GetInput(idx));
}

//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::SetIORegion (const ImageIORegion& region) 
{
  itkDebugMacro("setting IORegion to " << region );
  if ( m_PasteIORegion != region)
    {
    m_PasteIORegion = region;
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
      {
      std::list<LightObject::Pointer> allobjects = 
        ObjectFactoryBase::CreateAllInstance("itkImageIOBase");
      for(std::list<LightObject::Pointer>::iterator i = allobjects.begin();
          i != allobjects.end(); ++i)
        {
        ImageIOBase* io = dynamic_cast<ImageIOBase*>(i->GetPointer());
        msg << "    " << io->GetNameOfClass() << std::endl; 
        }
      }
      msg << "  You probably failed to set a file suffix, or" << std::endl;
      msg << "    set the suffix to an unsupported type." << std::endl;
      e.SetDescription(msg.str().c_str());
      e.SetLocation(ITK_LOCATION);
      throw e;
    }

  // NOTE: this const_cast<> is due to the lack of const-correctness
  // of the ProcessObject.
  InputImageType * nonConstImage = const_cast<InputImageType *>(input);

  // Update the meta data
  nonConstImage->UpdateOutputInformation();

  // Setup the ImageIO
  //
  m_ImageIO->SetNumberOfDimensions(TInputImage::ImageDimension);
  InputImageRegionType largestRegion = input->GetLargestPossibleRegion();
  const typename TInputImage::SpacingType& spacing = input->GetSpacing();
  const typename TInputImage::PointType& origin = input->GetOrigin();
  const typename TInputImage::DirectionType& direction = input->GetDirection();

  for(unsigned int i=0; i<TInputImage::ImageDimension; i++)
    {
    m_ImageIO->SetDimensions(i,largestRegion.GetSize(i));
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
  
  // configure compression
  m_ImageIO->SetUseCompression(m_UseCompression);

  // configure meta dictionary
  if( m_UseInputMetaDataDictionary )
    {
    m_ImageIO->SetMetaDataDictionary(input->GetMetaDataDictionary());
    }

  // Make sure that the image is the right type
  // confiugure pixel type
  typedef typename InputImageType::PixelType ScalarType;
  if( strcmp( input->GetNameOfClass(), "VectorImage" ) == 0 ) 
    {
      typedef typename InputImageType::InternalPixelType VectorImageScalarType;
      m_ImageIO->SetPixelTypeInfo( typeid(VectorImageScalarType) );
    
      typedef typename InputImageType::AccessorFunctorType AccessorFunctorType;
      m_ImageIO->SetNumberOfComponents( AccessorFunctorType::GetVectorLength(input) );
    }
  else
    {
      // Set the pixel and component type; the number of components.
      m_ImageIO->SetPixelTypeInfo(typeid(ScalarType));  
    }

  // Setup the image IO for writing.
  //
  m_ImageIO->SetFileName(m_FileName.c_str());

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  if (m_NumberOfStreamDivisions > 1 || m_UserSpecifiedIORegion) 
    {
    m_ImageIO->SetUseStreamedWriting(true);
    }
  
  

  ImageIORegion largestIORegion(TInputImage::ImageDimension);
  ImageIORegionAdaptor<TInputImage::ImageDimension>::
    Convert(largestRegion, largestIORegion);

  // this pasteIORegion is the region we are going to write
  ImageIORegion pasteIORegion;  
  if ( m_UserSpecifiedIORegion ) 
    {
    pasteIORegion = m_PasteIORegion;
    } 
  else 
    {
    pasteIORegion = largestIORegion;
    }

  // Determin the actual number of divisions of the input. This is determined
  // by what the ImageIO can do
  unsigned int numDivisions;
  
  // this may fail and throw an exception if the configuration is not supported
  numDivisions = m_ImageIO->GetActualNumberOfSplitsForWriting(m_NumberOfStreamDivisions,
                                                              pasteIORegion,
                                                              largestIORegion);

  
  /**
   * Loop over the number of pieces, execute the upstream pipeline on each
   * piece, and copy the results into the output image.
   */
  unsigned int piece;
  
  for (piece = 0;
       piece < numDivisions && !this->GetAbortGenerateData();
       piece++)
    {
      
    // get the actual piece to write
    ImageIORegion streamIORegion = m_ImageIO->GetSplitRegionForWriting(piece, numDivisions,
                                                                       pasteIORegion, largestIORegion);
      
    InputImageRegionType streamRegion;
    ImageIORegionAdaptor<TInputImage::ImageDimension>::
      Convert(streamIORegion, streamRegion);
    
    // execute the the upstream pipeline with the requested 
    // region for streaming
    nonConstImage->SetRequestedRegion(streamRegion);
    nonConstImage->PropagateRequestedRegion();
    nonConstImage->UpdateOutputData();
    
    // check to see if we got the largest possible region
    if (piece == 0) 
      {
      InputImageRegionType bufferedRegion = nonConstImage->GetBufferedRegion();
      if (bufferedRegion == nonConstImage->GetLargestPossibleRegion()) 
        {
        // if so, then just write the entire image
        itkDebugMacro("Requested stream region  matches largest region input filter may not support streaming well.");
        itkDebugMacro("Writer is not streaming now!");
        numDivisions = 1;
        streamRegion = nonConstImage->GetLargestPossibleRegion();
        ImageIORegionAdaptor<TInputImage::ImageDimension>::
          Convert(streamRegion, streamIORegion);
        }
      }

    m_ImageIO->SetIORegion(streamIORegion);
    
    // write the data
    this->GenerateData();
    
    this->UpdateProgress((float) piece / numDivisions );
    }

  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  this->ReleaseInputs();
}


//---------------------------------------------------------
template <class TInputImage>
void 
ImageFileWriter<TInputImage>
::GenerateData(void)
{
  const InputImageType * input = this->GetInput();
  typename InputImageType::Pointer cache;

  itkDebugMacro(<<"Writing file: " << m_FileName);
  

  // check that the image's buffered region is the same as
  // ImageIO is expecting and we requested
  ImageIORegion  ioRegion = m_ImageIO->GetIORegion();
  typename InputImageType::RegionType bufferedRegion = input->GetBufferedRegion();
  bool sameRegions = true;

  // we use this to compare a IORegion to just a Region
  if ( bufferedRegion.GetImageDimension() != ioRegion.GetImageDimension() ) 
    sameRegions = false;
  unsigned int i = 0;
  while (sameRegions && i < TInputImage::ImageDimension ) {
    if (ioRegion.GetSize(i) != long(bufferedRegion.GetSize(i)) ||
        ioRegion.GetIndex(i) != long(bufferedRegion.GetIndex(i))) 
      sameRegions = false;
    ++i;
  }
  
  
  // now extract the data as a raw buffer pointer
  const void* dataPtr = (const void*) input->GetBufferPointer();

  // before this test, bad stuff would happend when they don't match
  if (!sameRegions) 
    {
    if ( m_NumberOfStreamDivisions > 1 || m_UserSpecifiedIORegion) 
      {
      itkDebugMacro("Requested stream region does not match generated output");
      itkDebugMacro("input filter may not support streaming well");
      
      cache = InputImageType::New();
      typename InputImageType::RegionType cacheRegion;
      ImageIORegionAdaptor<TInputImage::ImageDimension>::
        Convert(ioRegion, cacheRegion); 
      cache->CopyInformation(input);
      cache->SetBufferedRegion(cacheRegion);
      cache->Allocate();

      typedef ImageRegionConstIterator<TInputImage> ConstIteratorType;
      typedef ImageRegionIterator<TInputImage>      IteratorType;
      
      ConstIteratorType in(input, cacheRegion);
      IteratorType out(cache, cacheRegion);
     
      // copy the data into a buffer to match the ioregion
      for (in.GoToBegin(), out.GoToBegin(); !in.IsAtEnd(); ++in, ++out) 
        {
        out.Set(in.Get());
        }
      
      dataPtr = (const void*) cache->GetBufferPointer();
      
      } 
    else 
      {
      ImageFileWriterException e(__FILE__, __LINE__);
      OStringStream msg;
      msg << "Did not get requested region!" << std::endl;
      msg << "Requested:" << std::endl;
      msg << ioRegion;
      msg << "Actual:" << std::endl;
      msg << bufferedRegion;
      e.SetDescription(msg.str().c_str());
      e.SetLocation(ITK_LOCATION);
      throw e;
    }
  } 


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

  os << indent << "IO Region: " << m_PasteIORegion << "\n";
  os << indent << "Number of Stream Divisions: " << m_NumberOfStreamDivisions << "\n";

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
