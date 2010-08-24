/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTxtTransformIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTxtTransformIO_h
#define __itkTxtTransformIO_h
#include "itkTransformIOBase.h"

namespace itk
{
class ITK_EXPORT TxtTransformIO:public TransformIOBase
{
public:
  typedef TxtTransformIO                Self;
  typedef TransformIOBase               Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef TransformBase                 TransformType;
  typedef Superclass::TransformPointer  TransformPointer;
  typedef Superclass::TransformListType TransformListType;
  /** Run-time type information (and related methods). */
  itkTypeMacro(TxtTransformIO, TransformIOBase);
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
  TxtTransformIO();
  virtual ~TxtTransformIO();
private:
  /** trim spaces and newlines from start and end of a string */
  std::string trim(std::string const & source, char const *delims = " \t\r\n");
};
}
#endif // __itkTxtTransformIO_h
