/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToMeshFilter.h
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
#ifndef __itkImageToMeshFilter_h
#define __itkImageToMeshFilter_h

#include "itkProcessObject.h"

namespace itk
{

/** \class ImageToMeshFilter
 * \brief 
 *
 * ImageToMeshFilter is the base class for all process objects that output
 * Mesh data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputMesh>
class ITK_EXPORT ImageToMeshFilter : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageToMeshFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef  ProcessObject  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageToMeshFilter, ProcessObject);


  /** 
   * Create a valid output
   */
  DataObject::Pointer  MakeOutput(unsigned int idx);


  /** 
   * Some Image related typedefs.
   */
  typedef   TInputImage                             InputImageType;
  typedef   typename InputImageType::Pointer        InputImagePointer;
  typedef   typename InputImageType::RegionType     InputImageRegionType; 
  typedef   typename InputImageType::PixelType      InputImagePixelType; 

  /** 
   * Some Mesh related typedefs.
   */
  typedef   TOutputMesh                             OutputMeshType;
  typedef   typename OutputMeshType::Pointer        OutputMeshPointer;

  /** 
   * Set the input image of this process object. 
   */
  void SetInput(unsigned int idx, InputImageType *input);

  /** 
   * Get the input image of this process object. 
   */
  InputImagePointer GetInput(unsigned int idx);

  /** 
   * Get the output Mesh of this process object. 
   */
  OutputMeshPointer GetOutput(void);


  /** 
   * Prepare the output
   */
  void GenerateOutputInformation(void);


     
protected:
  ImageToMeshFilter();
  ~ImageToMeshFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

 
private:
  ImageToMeshFilter(const ImageToMeshFilter&); //purposely not implemented
  void operator=(const ImageToMeshFilter&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToMeshFilter.txx"
#endif

#endif
