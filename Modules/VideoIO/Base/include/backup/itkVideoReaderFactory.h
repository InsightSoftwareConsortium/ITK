/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoReaderFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVideoReaderFactory_h
#define __itkVideoReaderFactory_h

#include "itkObject.h"

namespace itk
{
/** \class VideoReaderFactory
 * \brief Create instances of Video Reader objects using an object factory.
 */
template <class TImage>
class ITK_EXPORT VideoReaderFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef VideoReaderFactory             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */
  enum LIBRARY_USED { 
          ITK_USE_OPENCV,
          ITK_USE_VXL
          };

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VideoReaderFactory, Object);

  /** Convenient typedefs. **/
  typedef typename itk::VideoReaderBase<TImage>::Pointer VideoReaderBasePointer;

  /** Create the appropriate ImageIO depending on the particulars of the file.**/
  static VideoReaderBasePointer CreateVideoReader( LIBRARY_USED myChoice );

  /** Register Builtin factories **/
  static void RegisterBuiltInFactories();

protected:
  VideoReaderFactory();
  ~VideoReaderFactory();
private:
  VideoReaderFactory(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVideoReaderFactory.txx"
#endif

#endif
