/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLReader.txx
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

#include "itkVXLReader.h"
#include "vidl/vidl_convert.h"

namespace itk
{

template< typename TImage >
VXLReader< TImage >::VXLReader()
{
  this->m_VIDLImage = 0;
  this->m_VIDLFrame = 0;
  this->m_Reader = 0;
  this->m_ReaderOpen = false;
  this->m_FpS = 0;
  this->m_FrameTotal = 0;
  this->m_Width = 0;
  this->m_Height = 0;
  
  this->m_ImportFilter = ImportFilterType::New();
}

template< typename TImage >
void VXLReader< TImage >
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
bool VXLReader< TImage >
::SetNextFrameToRead (unsigned long frameNumber)
{
  if ( this->m_Reader->is_open() == true )
    {
    if (this->m_Reader->is_seekable () == true)
      {
      this->m_Reader->seek_frame (frameNumber);
      this->Modified();
      return true;
      }
    }
  return false;
}

template< typename TImage >
void VXLReader< TImage >::UpdateProperties ()
{
  this->m_CurrentFrame = this->m_Reader->frame_number();
}

template< typename TImage >
bool VXLReader< TImage >
::OpenReader(const char* filename)
{
  // load the file 
  this->m_Reader = new vidl_ffmpeg_istream();
  this->m_Reader->open(filename);

  if ( ! this->m_Reader->is_open() )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, the video specified hasn't been " 
      "correctly loaded. It can be because you don't haver ffmpeg "
      "(or vxl didn't find it");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    return false;
    }
  //Get the total number of frame 
  //Also set if the video is seekable or not (which can implies
  // a different way to read it...)
  this->m_FrameTotal = this->m_Reader->num_frames();
  if ( this->m_FrameTotal == 1 )
    {
    this->m_VideoSeekable = false;
    }
  else
    {
    this->m_VideoSeekable = true;
    }

  this->m_FpS = this->m_Reader->frame_rate();
  this->m_Width = this->m_Reader->width();
  this->m_Height = this->m_Reader->height();
  
  /*
  // create the image that will be used as based for creating the itk::Image
  this->m_CVImage = cvCreateImage( cvSize(this->m_Width,this->m_Height), IPL_DEPTH_8U, 1 );
  */

  // asserting that the video has been succesfully loaded
  this->m_ReaderOpen = true;
  return true;
}

template< typename TImage >
typename VXLReader<TImage>::ImageType::Pointer
VXLReader <TImage> :: Read()
{
  if ( this->m_Reader == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, when updating, " 
      "make sure you loaded the video first");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }

  //Setting the filter parameters
  this->m_Start.Fill(0);
  this->m_Size[0] = this->m_Width;
  this->m_Size[1] = this->m_Height;

  this->m_Region.SetIndex( this->m_Start );
  this->m_Region.SetSize( this->m_Size );
  this->m_ImportFilter->SetRegion( this->m_Region );

  double Origin[2] = {0.0,0.0};

  this->m_ImportFilter->SetOrigin( Origin );

  this->m_PixelFormat = this->m_Reader->format();

  this->m_Spacing[0] = vidl_pixel_format_bpp(this->m_PixelFormat)
    * vidl_pixel_format_num_channels(this->m_PixelFormat)/8;
  this->m_Spacing[1] = vidl_pixel_format_bpp(this->m_PixelFormat)
    * vidl_pixel_format_num_channels(this->m_PixelFormat)/8;
  this->m_ImportFilter->SetSpacing( this->m_Spacing );

  //Querying the frame
  this->m_VIDLImage = this->m_Reader->read_frame();

  if ( this->m_VIDLImage == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error when acquiring, " 
      "make sure you aren't out of boundaries");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }
  
  this->UpdateProperties();
 
  this->m_ImportFilter->SetImportPointer(reinterpret_cast<PixelType*>
    (this->m_VIDLImage->data()),this->m_VIDLImage->size(),false );
  this->m_ImportFilter->Update();

  return this->m_ImportFilter->GetOutput();
}

}; //namespace itk end
