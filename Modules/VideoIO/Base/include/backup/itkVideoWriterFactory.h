/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoWriterFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVideoWriterFactory_h
#define __itkVideoWriterFactory_h

#include "itkObject.h"

namespace itk
{
/** \class VideoWriterFactory
 * \brief Create instances of VideoWriter objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VideoWriterFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef VideoWriterFactory             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */
  enum LIBRARY_USED { 
          ITK_USE_OPENCV,
          ITK_USE_VXL
          };

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VideoWriterFactory, Object);

  /** Convenient typedefs. **/
  typedef typename itk::VideoWriterBase<TImage>::Pointer VideoWriterBasePointer;

  /** Create the appropriate ImageIO depending on the particulars of the file.**/
  static VideoWriterBasePointer CreateVideoWriter( LIBRARY_USED myChoice );

  /** Register Builtin factories **/
  static void RegisterBuiltInFactories();

protected:
  VideoWriterFactory();
  ~VideoWriterFactory();
private:
  VideoWriterFactory(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoWriterFactory.txx"
#endif

#endif