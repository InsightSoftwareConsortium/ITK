/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVReader.cxx
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

#include "itkOpenCVReader.h"

namespace itk
{

template< typename TImage >
OpenCVReader< TImage >::OpenCVReader()
{
  this->m_CVImage = 0;
  this->m_Temp = 0;
  this->m_Capture = 0;
  this->m_ReaderOpen = false;
  this->m_FpS = 0;
  this->m_FrameTotal = 0;
  this->m_CurrentFrame = 0;
  this->m_Width = 0;
  this->m_Height = 0;
  this->m_FourCC = 0;
  
  this->m_ImportFilter = ImportFilterType::New();
}

template< typename TImage >
void OpenCVReader< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  if (this->m_CVImage != NULL)
    {
    os << indent << "Image dimensions : ["<<this->m_CVImage->width<<","
        <<this->m_CVImage->height<<"]"<<std::endl;
    os << indent << "Origin : "<<this->m_CVImage->origin<<std::endl;
    os << indent << "Image spacing (in bits) : "<<this->m_CVImage->depth<<std::endl;
    os << indent << "Image Size : "<<this->m_CVImage->imageSize<<std::endl;
    os << indent << "Color model : "<<this->m_CVImage->colorModel
        <<" ("<<this->m_CVImage->nChannels<<" channels)"<<std::endl;
    }
}

template< typename TImage >
bool OpenCVReader< TImage >::OpenReader(const char* filename)
{
  // load the file 
  this->m_Capture = cvCaptureFromFile( filename );
 
  if ( ! this->m_Capture )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, the video specified hasn't been " 
      "correctly loaded");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    return false;
    }
  
  this->m_FrameTotal = static_cast<unsigned long>
    (cvGetCaptureProperty( this->m_Capture, CV_CAP_PROP_FRAME_COUNT ));
  this->m_FpS = static_cast<double>
    (cvGetCaptureProperty( this->m_Capture, CV_CAP_PROP_FPS ));

  this->m_Width = cvGetCaptureProperty( this->m_Capture, CV_CAP_PROP_FRAME_WIDTH );
  this->m_Height = cvGetCaptureProperty( this->m_Capture, CV_CAP_PROP_FRAME_HEIGHT );

  // create the image that will be used as based for creating the itk::Image
  this->m_CVImage = cvCreateImage( cvSize(this->m_Width,this->m_Height), IPL_DEPTH_8U, 1 );

  // asserting that the video has been succesfully loaded
  this->m_ReaderOpen = true;
  return true;
}

template< typename TImage >
typename OpenCVReader<TImage>::ImageType::Pointer
OpenCVReader<TImage> :: Read()
{
  if ( this->m_Capture == NULL || this->m_CVImage == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, when updating, " 
      "make sure you loaded the video first");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }

  //Setting the filter parameters
  this->m_Start.Fill(0);
  this->m_Size[0] = this->m_CVImage->width;
  this->m_Size[1] = this->m_CVImage->height;

  this->m_Region.SetIndex( this->m_Start );
  this->m_Region.SetSize( this->m_Size );
  this->m_ImportFilter->SetRegion( this->m_Region );

  if ( this->m_CVImage->origin == 0 )
    {
    this->m_Origin[0] = 0.0; // X coordinate
    this->m_Origin[1] = 0.0; // Y coordinate
    }
  else
    {
    this->m_Origin[0] = 0.0; // X coordinate
    this->m_Origin[1] = static_cast<double>(this->m_CVImage->height) ; // Y coordinate
    }
  this->m_ImportFilter->SetOrigin( this->m_Origin );

  this->m_Spacing[0] = (this->m_CVImage->depth*this->m_CVImage->nChannels)/8;
  this->m_Spacing[1] = (this->m_CVImage->depth*this->m_CVImage->nChannels)/8;
  this->m_ImportFilter->SetSpacing( this->m_Spacing );

  //Querying the frame
  this->m_Temp = cvQueryFrame(this->m_Capture);

  if ( this->m_Temp == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error when acquiring, " 
      "make sure you aren't out of boundaries");
    exception.SetLocation("LightVideoFileReader");
    throw exception;
    }
 
  this->UpdateProperties();

  //CV_RGB2GRAY: convert RGB image to grayscale 
  cvCvtColor(this->m_Temp,this->m_CVImage, CV_RGB2GRAY );
 
  this->m_ImportFilter->SetImportPointer(reinterpret_cast<PixelType*>
    (this->m_CVImage->imageData),this->m_CVImage->imageSize,false );
  this->m_ImportFilter->Update();
  
  return this->m_ImportFilter->GetOutput();
}

template< typename TImage >
bool OpenCVReader< TImage >::SetNextFrameToRead(unsigned long frameNumber)
{
  if (this ->m_Capture != NULL)
    {
    cvSetCaptureProperty(this->m_Capture,CV_CAP_PROP_POS_FRAMES,frameNumber);
    this->UpdateProperties();
    this->Modified();
    return true;
    }
  return false;
}

template< typename TImage >
void OpenCVReader< TImage >::UpdateProperties()
{
  this->m_CurrentFrame = 
    cvGetCaptureProperty(this->m_Capture,CV_CAP_PROP_POS_FRAMES);
  this->m_PositionInMSec = 
    cvGetCaptureProperty(this->m_Capture,CV_CAP_PROP_POS_MSEC);
  this->m_Ratio = 
    cvGetCaptureProperty(this->m_Capture,CV_CAP_PROP_POS_AVI_RATIO);
  this->m_FpS = 
    cvGetCaptureProperty(this->m_Capture,CV_CAP_PROP_FPS);
  this->m_FourCC =
    cvGetCaptureProperty(this->m_Capture,CV_CAP_PROP_FOURCC);
}

}; //namespace itk end
