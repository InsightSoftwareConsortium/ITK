/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  /** Standard class typedefs. */
  typedef ImageToMeshFilter  Self;
  typedef  ProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToMeshFilter, ProcessObject);

  /** Create a valid output. */
  DataObject::Pointer  MakeOutput(unsigned int idx);

  /** Some Image related typedefs. */
  typedef   TInputImage                             InputImageType;
  typedef   typename InputImageType::Pointer        InputImagePointer;
  typedef   typename InputImageType::RegionType     InputImageRegionType; 
  typedef   typename InputImageType::PixelType      InputImagePixelType; 

  /** Some Mesh related typedefs. */
  typedef   TOutputMesh                             OutputMeshType;
  typedef   typename OutputMeshType::Pointer        OutputMeshPointer;

  /** Set the input image of this process object.  */
  void SetInput(unsigned int idx, InputImageType *input);

  /** Get the input image of this process object.  */
  InputImagePointer GetInput(unsigned int idx);

  /** Get the output Mesh of this process object.  */
  OutputMeshPointer GetOutput(void);

  /** Prepare the output */
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
