/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatlabTransformIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMatlabTransformIO_h
#define __itkMatlabTransformIO_h
#include "itkTransformIOBase.h"

namespace itk
{
class ITK_EXPORT MatlabTransformIO:public TransformIOBase
{
public:
  typedef MatlabTransformIO             Self;
  typedef TransformIOBase               Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef TransformBase                 TransformType;
  typedef Superclass::TransformPointer  TransformPointer;
  typedef Superclass::TransformListType TransformListType;
  /** Run-time type information (and related methods). */
  itkTypeMacro(MatlabTransformIO, TransformIOBase);
  itkNewMacro(Self);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *);

  /** Determine the file type. Returns true if this ImageIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read();

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. The buffer is cast to a
   * pointer to the beginning of the image data. */
  virtual void Write();

protected:
  MatlabTransformIO();
  virtual ~MatlabTransformIO();
};
}
#endif // __itkMatlabTransformIO_h
