/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightVideoReader.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLightVideoFileReader_txx
#define __itkLightVideoFileReader_txx

#include "itkLightVideoFileReader.h"
#include "itkVideoReaderFactory.h"

#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{

template< typename TOutputImage, class ConvertPixelTraits >
LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::LightVideoFileReader()
{
  //Security stuff, we don't want to have good values 
  //if the user hasn't set them
  this->m_FileName = "";
  this->m_VideoReader = 0;
  
  //Declaration of default behavior
  this->m_VideoLoaded = false;
  this->m_UseOpenCV = true; 
}

/*
 * PrintSelf
 */
template< typename TOutputImage, class ConvertPixelTraits >
void
LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "File name : "<<this->m_FileName<<std::endl; 
} // end PrintSelf


template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::SetFileName(const char* filename)
{ 
  this->m_FileName = filename;
  this->m_VideoLoaded = false;
  this->Modified();
}

template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::UseOpenCV ( bool useOpenCV )
{
  if ( useOpenCV != this->m_UseOpenCV )
    {
    this ->m_UseOpenCV = useOpenCV;
    this->Modified();
    }
}

template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::GenerateData()
{  
  if ( ! this->m_VideoReader->IsReaderOpen() )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, when updating, " 
      "make sure you loaded the video first");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }

  this->GraftOutput(this->m_VideoReader->Read());

}

template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::GenerateOutputInformation()
{ 
  //We only need to do that once.
  if ( ! this->m_VideoLoaded )
    {
    this->LoadVideo();
    }
  
  //Retriveing the data for the region
  this->m_Start.Fill(0);
  this->m_Size[0] = this->m_VideoReader->GetWidth();
  this->m_Size[1] = this->m_VideoReader->GetHeight();  
  
  //Setting the filter's region
  this->m_Region.SetIndex( this->m_Start );
  this->m_Region.SetSize( this->m_Size );

  //Finally setting the region requested
  typename TOutputImage::Pointer output = this->GetOutput();
  output->SetLargestPossibleRegion(this->m_Region);
}

template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::KeepReading()
{
  this->Modified();
}

template< typename TOutputImage, class ConvertPixelTraits >
void LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::LoadVideo()
{
  //test existence and permission to read
  this->TestFileExistanceAndReadability();

  if ( this->m_UseOpenCV == true )
    {
    this->m_VideoReader = itk::VideoReaderFactory<TOutputImage>::CreateVideoReader(
          itk::VideoReaderFactory<TOutputImage>::ITK_USE_OPENCV);
    }
  else
    {
    this->m_VideoReader = itk::VideoReaderFactory<TOutputImage>::CreateVideoReader(
          itk::VideoReaderFactory<TOutputImage>::ITK_USE_VXL);
    }

  if ( this->m_VideoReader.IsNull() )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, the video hasn't been " 
      "correctly created");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }

  this->m_VideoReader->OpenReader(this->m_FileName.c_str());

  if ( ! this->m_VideoReader->IsReaderOpen() )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, the video specified hasn't been " 
      "correctly loaded");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }
    
  // asserting that the video has been succesfully loaded
  this->m_VideoLoaded = true;
  this->Modified();
}

template< class TOutputImage, class ConvertPixelTraits >
void
LightVideoFileReader< TOutputImage, ConvertPixelTraits >
::TestFileExistanceAndReadability()
{
  // Test if the file exists.
  if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
    {
    itk::ExceptionObject e(__FILE__, __LINE__);
    std::ostringstream       msg;
    msg << "The file doesn't exist. "
        << std::endl << "Filename = " << m_FileName
        << std::endl;
    e.SetDescription( msg.str().c_str() );
    throw e;
    return;
    }

  // Test if the file can be open for reading access.
  std::ifstream readTester;
  readTester.open( m_FileName.c_str() );
  if ( readTester.fail() )
    {
    readTester.close();
    std::ostringstream msg;
    msg << "The file couldn't be opened for reading. "
        << std::endl << "Filename: " << m_FileName
        << std::endl;
    itk::ExceptionObject e(__FILE__, __LINE__, msg.str().c_str(), ITK_LOCATION);
    throw e;
    return;
    }
  readTester.close();
}

} // end namespace itk

#endif