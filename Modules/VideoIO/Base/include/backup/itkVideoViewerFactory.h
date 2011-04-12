/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoViewerFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVideoViewerFactory_h
#define __itkVideoViewerFactory_h

#include "itkObject.h"

namespace itk
{
/** \class ImageIOFactory
 * \brief Create instances of ImageIO objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VideoViewerFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef VideoViewerFactory             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */
  enum LIBRARY_USED { 
          ITK_USE_OPENCV,
          ITK_USE_VXL
          };

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VideoViewerFactory, Object);

  /** Convenient typedefs. **/
  typedef typename itk::VideoViewerBase<TImage>::Pointer VideoViewerBasePointer;

  /** Create the appropriate ImageIO depending on the particulars of the file.**/
  static VideoViewerBasePointer CreateVideoViewer( LIBRARY_USED myChoice );

  /** Register Builtin factories **/
  static void RegisterBuiltInFactories();

protected:
  VideoViewerFactory();
  ~VideoViewerFactory();
private:
  VideoViewerFactory(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoViewerFactory.txx"
#endif

#endif