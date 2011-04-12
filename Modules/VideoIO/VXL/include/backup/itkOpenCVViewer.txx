/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVViewer.txx
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

#include "itkOpenCVViewer.h"

namespace itk
{

template< typename TImage >
OpenCVViewer< TImage >::OpenCVViewer()
{
  this->m_WaitTime = 40; //One image every 40 ms, it makes 25 images/s.
  //Security declaration, always better than to leave the variables "blank"
  this->m_OpenCVImage = 0;
  this->m_WindowName = "";
}


template< typename TImage >
bool OpenCVViewer< TImage >::Open (const char* WindowName)
{
  if ( cvNamedWindow(WindowName) != 0)
    {
    this->m_WindowName = WindowName;
    return true;
    }
  //if creation has failed
  itk::ExceptionObject e;
  e.SetDescription("Error while trying to create the window");
  e.SetLocation("itkOpenCVViewer");
  throw e;
  return false;
}

template< typename TImage >
bool OpenCVViewer< TImage >::Close (const char* WindowName)
{
  if (this->m_WindowName != "")
    {
    cvDestroyWindow(this->m_WindowName.c_str());
    this->m_WindowName = "";
    return true;    
    }
  //if the destroying failed
  return false;
}

template< typename TImage >
bool OpenCVViewer< TImage >::Play(typename itk::Image<PixelType,2>::Pointer ITKImage)
{
  //Deciding if we should display the image or not is done 
  // at an higher level.

  //Convert the image
  this->InitImage(ITKImage);
  //Display the video
  cvShowImage(this->m_WindowName.c_str(),this->m_OpenCVImage);
  return true;
}

template< typename TImage >
void OpenCVViewer< TImage >::Wait()
{
  //we don't really care about the key for now
  //Maybe we could int other implementation
  cvWaitKey(this->m_WaitTime);
}

template< typename TImage >
void OpenCVViewer< TImage >::Wait (int MSec)
{
  //we don't really care about the key for now
  //Maybe we could int other implementation
  cvWaitKey(MSec);
}

template< typename TImage >
void OpenCVViewer< TImage >::SetWaitTime(int MSec)
{
  this->m_WaitTime = MSec;
}
  
template< typename TImage >
void OpenCVViewer< TImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  //Quite poor so far, we'll see later.
  Superclass::PrintSelf(os,indent);
  os<<indent<<"Wait time : "<<this->m_WaitTime<<std::endl;
}

template <typename TImage>
void OpenCVViewer< TImage >::
InitImage(typename itk::Image<PixelType,2>::Pointer ITKImage)
{
  //compute the pixel depth
  int depth = sizeof(PixelType)*8;
  
  //Get the image in region
  itk::ImageRegion<2> region = ITKImage->GetLargestPossibleRegion();
  itk::Size<2> itkSize = region.GetSize();
  
  CvSize CVSize;
  CVSize.width = itkSize[0];
  CVSize.height = itkSize[1];

  //Create the header
  this->m_OpenCVImage = cvCreateImageHeader(CVSize,depth,1);
  // and Retrieve the data so we don't reload the data
  //We instead use the same buffer ( same buffer -> same image )
  cvSetData(this->m_OpenCVImage,const_cast<PixelType*>(ITKImage->GetBufferPointer()),CVSize.width);
}

} //end namespace itk
