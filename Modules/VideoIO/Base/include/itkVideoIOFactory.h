/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVideoIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVideoIOFactory_h
#define __itkVideoIOFactory_h

#include "itkObject.h"
#include "itkVideoIOBase.h"


namespace itk
{

/** \class VideoIOFactory
 * \brief Create instances of VideoIO objects using an object factory.
 */
class ITK_EXPORT VideoIOFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef VideoIOFactory             Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */
  //enum LIBRARY_USED { 
  //        ITK_USE_OPENCV,
  //        ITK_USE_VXL
  //        };

  /** Mode in which the VideoIO is intended to be used */
  typedef enum { ReadFileMode, ReadCameraMode, WriteMode } IOModeType;

  /** Runtime type information (and related methods). **/
  itkTypeMacro(VideoIOFactory, Object);

  /** Create the appropriate ImageIO depending on the particulars of the file.
   *  Note: arg can either be a path for reading/writing from/to a file or a
   *        a string containing an integer to use for a cameraID if reading
   *        from a camera
   */
  static VideoIOBase::Pointer CreateVideoIO( IOModeType mode, const char* arg );

  /** Register Builtin factories **/
  static void RegisterBuiltInFactories();

protected:
  VideoIOFactory();
  ~VideoIOFactory();
private:
  VideoIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};

} // end namespace itk


#endif
