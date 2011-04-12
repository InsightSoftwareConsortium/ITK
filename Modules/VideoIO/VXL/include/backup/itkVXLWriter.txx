/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoWriterBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined( _MSC_VER )
#pragma warning ( disable : 4786 )
#endif

#include "itkVXLWriter.h"
#include "vidl/vidl_convert.h"

namespace itk
{

template< typename TImage >
VXLWriter< TImage >::VXLWriter()
{
  this->m_VIDLFrame = 0;
  this->m_WriterOpen = false;
  this->m_FpS = 25;
  this->m_Width = 0;
  this->m_Height = 0;
  this->m_Encoder = vidl_ffmpeg_ostream_params::DEFAULT;
}

template< typename TImage >
void VXLWriter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  /*if (this->m_CVImage != NULL)
    {
    os << indent << "Image dimensions : ["<<this->m_CVImage->width<<","
        <<this->m_CVImage->height<<"]"<<std::endl;
    os << indent << "Origin : "<<this->m_CVImage->origin<<std::endl;
    os << indent << "Image spacing (in bits) : "<<this->m_CVImage->depth<<std::endl;
    os << indent << "Image Size : "<<this->m_CVImage->imageSize<<std::endl;
    os << indent << "Color model : "<<this->m_CVImage->colorModel
        <<" ("<<this->m_CVImage->nChannels<<" channels)"<<std::endl;
    }*/
}

template< typename TImage >
bool VXLWriter< TImage >
::Close(const char* filname)
{
  if ( this->m_Writer != 0 )
    {
    this->m_Writer->close();
    this->m_Writer = 0;
    return true;
    }
  else
    {
    return false;
    }
}

template< typename TImage >
bool VXLWriter< TImage >
::OpenWriter(const char* filename, typename itk::Image<typename TImage::PixelType,2>::Pointer ITKImage)
{
  //Get the image in region so that we can get the size of the image to write
  this->m_Region = ITKImage->GetLargestPossibleRegion();
  this->m_Size = this->m_Region.GetSize();
  
  vidl_ffmpeg_ostream_params parameters ;
  parameters.frame_rate_ = 29.95;//25 by default, can be changed
  //parameters.bit_rate_ = 5000;//5000 by default in VXL
  //Since I'm not sure if changing it is a good idea, I leave the default
  parameters.ni_ = this->m_Size[0];//Set the width
  parameters.nj_ = this->m_Size[1];//Set the Height
  parameters.encoder_ = this->m_Encoder;//DVVIDEO by default can be changed
  //parameter.file_format_ = vidl_ffmpeg_ostream_params::GUESS;//No choice...
  
  this->m_Writer = new vidl_ffmpeg_ostream(filename,parameters);

  return this->m_Writer->open();
}

template< typename TImage >
bool VXLWriter < TImage >::Write
(typename ImageType::Pointer ITKImage)
{
  if ( ! this->m_Writer->is_open() )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, the video specified hasn't been " 
      "correctly loaded. It can be because you don't have ffmpeg "
      "(or vxl didn't find it)");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    return false;
    }

  vidl_pixel_format pixelFormat;

  switch (sizeof(typename TImage::PixelType))
  {
  case 1 :
    pixelFormat = vidl_pixel_format_from_string("VIDL_PIXEL_FORMAT_MONO_1");
    break;
  case 8 :
    pixelFormat = vidl_pixel_format_from_string("VIDL_PIXEL_FORMAT_MONO_8");
    break;
  case 16 :
    pixelFormat = vidl_pixel_format_from_string("VIDL_PIXEL_FORMAT_MONO_16");
    break;
  default :
    itk::ExceptionObject exception;
    exception.SetDescription("Error, VXL Writer doesn't support type that have more than 16 bits");
    exception.SetLocation("VideoFileWriter");
    throw exception;
    return false;
  }
    
  
  vidl_shared_frame  *VIDLFrame = new vidl_shared_frame(
    const_cast<PixelType*>(ITKImage->GetBufferPointer()),
    static_cast<unsigned int>(this->m_Size[0]),
    static_cast<unsigned int>(this->m_Size[1]),
    pixelFormat);
  
  this->m_Writer->write_frame(VIDLFrame);

  return true;
}


}; //namespace itk end
