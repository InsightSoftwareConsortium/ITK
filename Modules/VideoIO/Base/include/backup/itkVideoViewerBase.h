/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoViewerBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLightProcessObject.h"
#include "itkExceptionObject.h"
#include "itkImage.h"

#ifndef __itkVideoViewerBase_h
#define __itkVideoViewerBase_h

namespace itk
{

/** \class VideoViewerBase
 * \brief Create a pop up window that display your video
 * 
 *  Abstract base class for the viewer class. All viewer should 
 *  inherits from this class and reimplement its method.
 *    
 *  The main goal of this class is to propose create an interface
 *  between the iktVideoViewer which takes care of the pipeline
 *  implementation of itk and the Viewer like itkOpenCVViewer or 
 *  itkVXLViewer that are doing the actual displaying.
 *  This way the itkVideoViewer can operate regardless the library
 *  you want to use (as long as you use a least one...) and you can implement
 *  your own Viewer that uses your own library.
 *
 *  \sa VideoViewer
 *  \sa OpenCVViewer
 *  \sa VXLViewer
 */

template <class TImage>
class ITK_EXPORT VideoViewerBase : public LightProcessObject
{
public:
  /** Standard class typedefs. **/
  typedef VideoViewerBase                           Self;
  typedef LightProcessObject                        Superclass;
  typedef SmartPointer< Self >                      Pointer;

  /** Convinient typedef **/
  typedef TImage                                    ImageType;
  typedef typename ImageType::PixelType             PixelType;

  /** Run-time type information (and related methods). **/
  itkTypeMacro(VideoViewerBase, Superclass);

  /** Try to open a windows **/
  /** Return true if in case of a success, false for a faillure **/
  /** Intended to be overloaded by subclasses **/
  virtual bool Open (const char* WindowName) = 0;

  /** Try to close a viewer **/
  /** Return true if in case of a success, false for a faillure **/
  virtual bool Close (const char* WindowName) = 0;

 /** Display the input image **/
  virtual bool Play(typename itk::Image<PixelType,2>::Pointer ITKImage) = 0;

  /** Wait the time specified by the SetWaitTime() **/
  virtual void Wait() = 0;
  /** Wait the time specified **/
  virtual void Wait (int MSec) = 0;

  /** Accessors **/
  virtual int GetWaitTime () = 0;
  virtual void SetWaitTime(int MSec) = 0;
  
protected:  
  VideoViewerBase();
  ~VideoViewerBase(){};

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  VideoViewerBase(const Self &);    //purposely not implemented
  void operator=(const Self &); //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoViewerBase.txx"
#endif

#endif // __itkVideoViewerBase_h
