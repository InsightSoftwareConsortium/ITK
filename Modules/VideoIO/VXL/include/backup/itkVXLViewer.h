/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLViewer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkVideoViewerBase.h"
#include <vcl_iostream.h>
#include "vgui/vgui.h"
#include "vgui/vgui_toolkit.h"
#include "vgui/vgui_image_tableau.h"
#include "vgui/vgui_viewer2D_tableau.h"
#include "vgui/vgui_shell_tableau.h"

#ifndef __itkVXLViewer_h
#define __itkVXLViewer_h

namespace itk
{


template <class TImage>
class ITK_EXPORT VXLViewer : public VideoViewerBase<TImage>
{
public:
  /** Standard class typedefs. **/
  typedef VXLViewer                                  Self;
  typedef LightProcessObject                        Superclass;
  typedef SmartPointer< Self >                      Pointer;

  /** Convinient typedef **/
  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;

  /** Run-time type information (and related methods). **/
  itkTypeMacro(VXLViewer, Superclass);

  /** New macro **/
  itkNewMacro(Self);

  /** Try to open a windows **/
  /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  bool Open (const char *WindowName);

  /** Try to close a viewer **/
  /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  bool Close (const char *WindowName);

 /** Display the input image **/
  bool Play (typename itk::Image<PixelType,2>::Pointer ITKImage);

  /** Wait the time specified by the SetWaitTime() **/
  void Wait ();
  /** Wait the time specified **/
  void Wait (int MSec);

  /** Accessors **/
  int GetWaitTime () {return this->m_WaitTime;};
  void SetWaitTime(int MSec);
  
protected:  
  VXLViewer();
  ~VXLViewer(){};

  void PrintSelf(std::ostream & os, Indent indent) const;
  int             m_WaitTime;
  //vgui_shell_tableau_new            *m_GUI;
  //vidl_shared_frame  *m_VIDLFrame;


private:
  VXLViewer(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  void InitImage(typename itk::Image<PixelType,2>::Pointer ITKImage);

  std::string       m_WindowName;
  //int m_Width;
  //int m_Height;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVXLViewer.txx"
#endif

#endif // __itkVXLViewer_h
