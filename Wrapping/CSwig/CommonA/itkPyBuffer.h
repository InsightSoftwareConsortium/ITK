/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPyBuffer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPyBuffer_h
#define _itkPyBuffer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkImportImageFilter.h"

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#include <Python.h>
#include <arrayobject.h>



namespace itk
{

/** \Class PyBuffer
 *  \brief Helper class for converting C buffers into python arrays.
 * 
 *  This class will receive a C buffer and create the equivalen python
 *  array. This permits to pass image buffers into python arrays from
 *  the Numeric python package.
 *
 */

template <typename TImage>
class PyBuffer : public Object
{
public:
  ///! Standard "Self" typedef.
  typedef PyBuffer         Self;

  ///! Smart pointer typedef support.
  typedef SmartPointer<Self>  Pointer;

  ///! Run-time type information (and related methods).
  itkTypeMacro(PyBuffer,Object);

  ///! Method for creation through the object factory.
  itkNewMacro(Self);

  /// Type of the image from where the buffer will be converted
  typedef TImage                              ImageType;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::SizeType        SizeType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::RegionType      RegionType;
  typedef typename ImageType::PointType       PointType;
  typedef typename ImageType::SpacingType     SpacingType;


   /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);
 
  /// Type of the import image filter
  typedef ImportImageFilter< PixelType, 
                             ImageDimension >   ImporterType;

  typedef typename ImporterType::Pointer   ImporterPointer;
  
  /** 
   * Get an Array with the content of the image buffer
   */
  PyObject * GetArrayFromImage(const ImageType * image);

  /** 
   * Get an ITK image from a Python array
   */
  const ImageType * GetImageFromArray( PyObject *obj );


protected:
  PyBuffer();
  ~PyBuffer();
  PyBuffer(const Self&);     // Not implemented.
  void operator=(const Self&); // Not implemented.

private:
  PyObject          *obj;
  ImporterPointer    m_Importer;
};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPyBuffer.txx"
#endif

#endif // _itkPyBuffer_h

