/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageReader.h
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
#ifndef __itkVTKImageReader_h
#define __itkVTKImageReader_h

#include "itkImageSource.h"

namespace itk
{

/** \class VTKImageReader
 * \brief Read VTK-formatted image files.
 *
 * VTKImageReader reads VTK-formatted data files. This class requires
 * that the VTK dataset type is STRUCTURED_POINTS, and only the scalar
 * point data is read (1-4 components).
 *
 * \ingroup IOFilters
 */
template <class TOutputImage>
class ITK_EXPORT VTKImageReader : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VTKImageReader        Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(VTKImageReader,ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Specify the name of the input file.
   */
  itkSetStringMacro(FileName);
  
  /** 
   * Get the name of the input file.
   */
  itkGetStringMacro(FileName);
  
protected:
  VTKImageReader();
  ~VTKImageReader() {};
  VTKImageReader(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  void GenerateData();

private:
  std::string m_FileName;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKImageReader.txx"
#endif

#endif
