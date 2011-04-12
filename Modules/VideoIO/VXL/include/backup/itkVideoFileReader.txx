/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFaceDetectionFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVideoFileReader_txx
#define __itkVideoFileReader_txx

#include "itkVideoFileReader.h"
#include "itkExceptionObject.h"

namespace itk
{

template< typename TOutputImage >
VideoFileReader< TOutputImage >
::VideoFileReader()
{
  this->InitializeProperties();
}

template< typename TOutputImage >
void VideoFileReader< TOutputImage >
::InitializeProperties()
{
  this->m_FrameRequested = 0;
  this->m_PositionInMSec = 0;
  this->m_Ratio = 0;
  this->m_FrameWidth = 0;
  this->m_FrameHeight = 0;
  this->m_FpS = 25;
  this->m_NextFrameIsFrameRequested = false;
}

/*
 * PrintSelf
 */
template< typename TOutputImage >
void
VideoFileReader< TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Frame requested : "<<this->m_FrameRequested<< std::endl;
  os << indent << "Position in milliseconds : "<<this->m_PositionInMSec<< std::endl;
  os << indent << "Ratio of video elapsed : "<<this->m_Ratio<< std::endl;
  os << indent << "Frame width : "<<this->m_FrameWidth<< std::endl;
  os << indent << "Frame Height : "<<this->m_FrameHeight<< std::endl;
  os << indent << "Number of frame per second : "<<this->m_FpS<< std::endl;
  
} // end PrintSelf

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::GenerateData()
{
  if (Superclass::m_VideoLoaded == false )
    {
    this->LoadVideo();
    }
  if ( this->m_NextFrameIsFrameRequested == true )
    {
    if ( Superclass::m_VideoReader->SetNextFrameToRead(this->m_FrameRequested) == false )
      {
      //Means that hte video is non-seekable or out of boundaries
      itk::ExceptionObject exception;
      exception.SetDescription("Error, Either the video is non-seekable" 
        "or you're out of boundaries");
      exception.SetLocation("VideoFileReader");
      throw exception;
      }
    } 
  this->UpdateProperties();
  Superclass::GenerateData();
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::SetFileName(const char *filename)
{
  this->InitializeProperties();
  Superclass::SetFileName(filename);
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::LoadVideo()
{
  Superclass::LoadVideo();
  this->UpdateProperties();
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::PlayInput(unsigned long frame)
{
  this->SetFrameRequested(frame);
  this->PlayInput();
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::PlayInput()
{
  /*int result;
  cvShowImage("Reader Input",cvQueryFrame(Superclass::m_Capture));
  result=cvWaitKey(1000/this->m_FpS);*/
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::PlayOutput (unsigned long frame)
{/*
  this->SetFrameRequested(frame);
  this->PlayOutput();*/
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::PlayOutput ()
{/*
  int result;
  cvShowImage("Reader output",Superclass::m_CVImage);
  result=cvWaitKey(1000/this->m_FpS);*/
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::SetFrameRequested(unsigned long frame)
{
  this->m_FrameRequested = frame;
  this->Modified();
}

template< typename TOutputImage >
void VideoFileReader<TOutputImage>
::UpdateProperties()
{
  if ( Superclass::m_VideoLoaded == true )
    {
    this->m_FrameHeight = Superclass::m_VideoReader->GetHeight() ;
    this->m_FrameWidth = Superclass::m_VideoReader->GetWidth();
    this->m_FrameRequested = Superclass::m_VideoReader->GetHeight();
    this->m_PositionInMSec = Superclass::m_VideoReader->GetPositionInMSec();
    this->m_Ratio = Superclass::m_VideoReader->GetRatio();
    this->m_FpS = Superclass::m_VideoReader->GetFpS();
    //this->m_FourCC = Superclass::m_VideoReader->GetFourCC();
    }
}


} // end namespace itk

#endif