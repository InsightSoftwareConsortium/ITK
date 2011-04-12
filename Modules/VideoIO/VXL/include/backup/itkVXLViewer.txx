/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLViewer.txx
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

#include "itkVXLViewer.h"

namespace itk
{

template< typename TImage >
VXLViewer< TImage >::VXLViewer()
{
  this->m_WaitTime = 40; //One image every 40 ms, it makes 25 images/s.
  this->m_WindowName = "";
  //this->m_GUI = 0;
}


template< typename TImage >
bool VXLViewer< TImage >::Open (const char* WindowName)
{
  //int argc = 0;
  //char *argv[1];
  //find the "toolkit" itself
  //vgui::init(argc, argv);

  //this->m_WindowName = WindowName;
  //return true;
  return false;
}

template< typename TImage >
bool VXLViewer< TImage >::Close (const char* WindowName)
{
  if (this->m_WindowName != "")
    {
    //cvDestroyWindow(this->m_WindowName.c_str());
    this->m_WindowName = "";
    return true;    
    }
  return false;
}

template< typename TImage >
bool VXLViewer< TImage >::Play(typename itk::Image<PixelType,2>::Pointer ITKImage)
{
  this->InitImage(ITKImage);
  //try to see if a player is opened or not :
  
  //this->m_GUI = new vgui_shell_tableau_new(this->m_VIDLFrame);
  //return vgui::run(this->m_GUI, this->m_Width, this->m_Height, this->m_WindowName.c_str());
  return false;
}

template< typename TImage >
void VXLViewer< TImage >::Wait()
{
  //we don't really care about the key for now
  //int key = cvWaitKey(this->m_WaitTime);
}

template< typename TImage >
void VXLViewer< TImage >::Wait (int MSec)
{
  //int key = cvWaitKey(MSec);
}

template< typename TImage >
void VXLViewer< TImage >::SetWaitTime(int MSec)
{
  this->m_WaitTime = MSec;
}
  
template< typename TImage >
void VXLViewer< TImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os<<indent<<"Wait time : "<<this->m_WaitTime<<std::endl;
}

template <typename TImage>
void VXLViewer< TImage >::
InitImage(typename itk::Image<PixelType,2>::Pointer ITKImage)
{
  /*vidl_pixel_format pixelFormat;

  switch (sizeof(TImage::PixelType))
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

  //Get the image in region so that we can get the size of the image to write
  itk::Region<2> Region = ITKImage->GetLargestPossibleRegion();
  itk::Size<2> Size = Region.GetSize();

  this->m_With = static_cast<unsigned int>(Size[0]);
  this->m_Height = static_cast<unsigned int>(Size[1]);
  
  this->m_VIDLFrame = new vidl_shared_frame(
    const_cast<TImage::PixelType*>(ITKImage->GetBufferPointer()),
    this->m_With,
    this->m_Height,
    pixelFormat);
  */
}

}