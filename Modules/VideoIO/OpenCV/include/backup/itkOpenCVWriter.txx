/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVWriter.xx
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

#include "itkOpenCVWriter.h"

namespace itk
{

template< typename TImage >
OpenCVWriter< TImage >::OpenCVWriter()
{
  this->m_CVImage = 0;
  this->m_Temp = 0;
  this->m_Writer = 0;
  this->m_WriterOpen = false;
  this->m_FpS = 25;
  this->m_Width = 0;
  this->m_Height = 0;
  this->m_FourCC = CV_FOURCC('P','I','M','1');
}

template< typename TImage >
void OpenCVWriter< TImage >
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
bool OpenCVWriter< TImage >
::Close(const char* filname)
{
  if ( this->m_Writer != 0 )
    {
    cvReleaseVideoWriter(&this->m_Writer);
    this->m_Writer = 0;
    return true;
    }
  else
    {
    return false;
    }
}

template< typename TImage >
bool OpenCVWriter< TImage >
::OpenWriter(const char* filename, typename itk::Image<typename TImage::PixelType,2>::Pointer ITKImage)
{
  //compute the pixel depth
  int depth = sizeof(typename TImage::PixelType)*8;
  
  //Get the image in region
  this->m_Region = ITKImage->GetLargestPossibleRegion();
  this->m_Size = this->m_Region.GetSize();
  
  CvSize size;
  size.width = this->m_Size[0];
  size.height = this->m_Size[1];

  //Create the header
  this->m_Temp = cvCreateImageHeader(size,depth,1);
  this->m_CVImage = cvCreateImage(size,depth, 3);

  if ( this->m_Temp == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, when creating the video");
    exception.SetLocation("LightVideoFileWriter");
    throw exception;
    return false;
    }
  else
    {
    //Creating the writer 
    this->m_Writer = cvCreateVideoWriter(
      filename,this->m_FourCC,
      this->m_FpS,size,1); 
    this->m_WriterOpen = true;
    return true;
    }
}

template< typename TImage >
bool OpenCVWriter< TImage >::Write
(typename ImageType::Pointer ITKImage)
{
  if ( this->m_Writer == NULL )
    {
    itk::ExceptionObject exception;
    exception.SetDescription("Error, when using the video");
    exception.SetLocation("LightVideoFileWriter");
    throw exception;
    }
  
   //Retrieve the data so we don't reload the data
  //We instead use the same buffer ( same buffer -> same image )
  cvSetData(this->m_Temp,const_cast<PixelType*>(ITKImage->GetBufferPointer()),this->m_Temp->widthStep);

  //We need to convert it to a RGB image
  cvCvtColor(this->m_Temp, this->m_CVImage, CV_GRAY2RGB);

  cvWriteFrame(this->m_Writer,this->m_CVImage);
  
  return true;
}

}; //namespace itk end
