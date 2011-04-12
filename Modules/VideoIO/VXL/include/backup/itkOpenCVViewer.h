/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVViewer.h
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
#include "cv.h"
#include "highgui.h"

#ifndef __itkOpenCVViewer_h
#define __itkOpenCVViewer_h

namespace itk
{

/** \class OpenCVViewer
 * \brief Base class for displaying images and wait 
 *
 *  The purpose of this class is to be used in others higher level
 *  class for playing video or images. 
 *  It is not intented to stand by itself. 
 *  Consequently the implementation offers fairly simple ways to
 *  display images.
 * 
 * \sa VideoViewerBase
 * \sa VideoViewer
 *
 * \ingroup OpenCVFilters
 */

template <class TImage>
class ITK_EXPORT OpenCVViewer : public VideoViewerBase<TImage>
{
public:
  /** Standard class typedefs. **/
  typedef OpenCVViewer                        Self;
  typedef LightProcessObject                  Superclass;
  typedef SmartPointer< Self >                Pointer;

  /** Convinient typedef **/
  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;

  /** Run-time type information (and related methods). **/
  itkTypeMacro(OpenCVViewer, Superclass);

  /** New macro **/
  itkNewMacro(Self);

  /** Try to open a windows **/
  /** Return true if in case of a success, false for a faillure **/
  bool Open (const char *WindowName);

  /** Try to close a viewer **/
  /** Return true if in case of a success, false for a faillure **/
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
  OpenCVViewer();
  ~OpenCVViewer(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

  int                               m_WaitTime;
  IplImage                          *m_OpenCVImage;

private:

  OpenCVViewer(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented
  
  //Do the conversion from ITK image format to OpenCV Image format
  // and put it in m_OpenCVImage
  void InitImage(typename itk::Image<PixelType,2>::Pointer ITKImage);
  //Holds the name of the window
  std::string       m_WindowName;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOpenCVViewer.txx"
#endif

#endif // __itkOpenCVViewer_h
