/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPNGImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkPNGImageIO_h
#define __itkPNGImageIO_h

#include "itkImageIOBase.h"

namespace itk
{

class ITK_EXPORT PNGImageIO : public ImageIOBase
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef PNGImageIO            Self;
  typedef SmartPointer<Self>  Pointer;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageIOBase  Superclass;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(PNGImageIO, Superclass);

  /**
   * Determine the file type. Returns true if this ImageIO can read the
   * file specified.
   */
  virtual bool CanReadFile(const char*) ;

  /**
   * Get the type of the pixel. 
   */
  virtual const type_info& GetPixelType() const;

  /**
   * Loads the data from disk into the memory buffer provided.
   */
  virtual void Load(void* buffer);

  /**
   * Get the image origin.
   */
  virtual const double* GetOrigin() const;

  /**
   * Get the image spacing.
   */
  virtual const double* GetSpacing() const;

  /**
   * Compute the size (in bytes) of the components of a pixel. For
   * example, and RGB pixel of unsigned char would have a 
   * component size of 1 byte.
   */
  virtual unsigned int GetComponentSize() const;
protected:
  PNGImageIO();
  ~PNGImageIO();
  PNGImageIO(const Self&) {}
  void operator=(const Self&) {}
  
  bool ReadHeader(const char* fname);
  void PrintSelf(std::ostream& os, Indent indent) const;
  ComponentType m_PNGPixelType;
  double m_Spacing[2];
  double m_Origin[2];
};

} // end namespace itk

#endif // __itkPNGImageIO_h
