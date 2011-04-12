/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoViewer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVideoViewer_txx
#define __itkVideoViewer_txx

#include "itkVideoViewer.h"
#include "itkVideoViewerFactory.h"
#include "itkVideoFileWriter.h"

namespace itk
{ 

template< typename TInputImage >
VideoViewer< TInputImage>::VideoViewer()
{
  //Enables to know if a Viewer has been opened yet
  this->m_ViewerExists = false;
  //Enables you to know if the Viewer has been closed upon the user will
  this->m_ViewerClosed = false;
  //If you use OpenCV (or VXL when false)
  this->m_UseOpenCV = true;
  //Security, it's always better to put zero value than to leave it "blank"
  this->m_VideoViewer = 0;
  this->m_WindowName = "";
}

template< typename TInputImage >
bool VideoViewer< TInputImage>::SetWaitTime(int MSec)
{ 
  //We can only set the time if the objects exists
  if (this->m_ViewerExists == true )
    {  
    this->m_VideoViewer->SetWaitTime(MSec);
    this->Modified();
    return true;
    }
  return false;
}

template< typename TInputImage >
void VideoViewer< TInputImage>::UseOpenCV ( bool useOpenCV )
{
  //If you try to change the type while playing
  //(kind of brutal)
  if (this->m_UseOpenCV != useOpenCV && this->m_ViewerExists)
    {
    this->Close();
    itk::ExceptionObject e;
    e.SetDescription("Error, You cannot change the type of Viewer while having a reader opened");
    e.SetLocation("VideoViewer");
    throw e;
    }
  this->m_UseOpenCV = useOpenCV;
  this->Modified();
}

template< typename TInputImage >
void VideoViewer< TInputImage>::Wait()
{
  this->m_VideoViewer->Wait();
}

template< typename TInputImage >
void VideoViewer< TInputImage>::Close()
{
  //If a player is opened
  if (this->m_ViewerExists == true)
    {
    this->m_VideoViewer->Close(this->m_WindowName.c_str());
    this->m_ViewerExists = false;
    this->m_ViewerClosed = true;
    this->Modified();
    }
}

template< typename TInputImage >
void VideoViewer< TInputImage>::Open()
{
  //If the user don't specify a name, we use the name of the class as default
  this->Open(this->GetNameOfClass());
  this->m_WindowName = this->GetNameOfClass();
}

template< typename TInputImage >
void VideoViewer< TInputImage>::Open(const char* WindowName)
{
  //If the video isn't opened yet
  if (this->m_ViewerExists == false)
    {
    if (this->m_UseOpenCV == true)
      {
      this->m_VideoViewer = VideoViewerFactory<TInputImage>::CreateVideoViewer
          (VideoViewerFactory<TInputImage>::ITK_USE_OPENCV);
      this->m_VideoViewer->Open(WindowName);
      }
    else
      {
      this->m_VideoViewer = VideoViewerFactory<TInputImage>::CreateVideoViewer
          (VideoViewerFactory<TInputImage>::ITK_USE_VXL);
      this->m_VideoViewer->Open(WindowName);
      }
    this->m_ViewerExists = true;
    this->m_ViewerClosed = false;
    this->Modified();
    }
}

template< typename TInputImage >
void VideoViewer< TInputImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  //Quite poor so far, we'll see later
  Superclass::PrintSelf(os,indent);
  os<<indent<<"Viewer open : "<<this->m_ViewerExists<<std::endl;
  os<<indent<<"UseOpenCV : "<<this->m_UseOpenCV<<std::endl;
}
  
template< typename TInputImage >
void VideoViewer< TInputImage>
::GenerateData()
{
  InputImageType* inputPtr = const_cast<InputImageType*>(this->GetInput());
  //To make sure the data is here :
  inputPtr->Update();

  //if the viewer hasn't been closed manually
  if (this->m_ViewerClosed == false )
    {
    //if a windowName has been set
    if (this->m_WindowName != "")
      {
      this->Open(this->m_WindowName.c_str());
      }
    else
      {
      this->Open();
      }
    //Then we can read and wait
    this->m_VideoViewer->Play( inputPtr );
    this->m_VideoViewer->Wait();
    }
  //finaly we graft the input into the output. 
  //The only difference is the possible delay. 
  this->GraftOutput( inputPtr );
}

} //end namespace itk

#endif
