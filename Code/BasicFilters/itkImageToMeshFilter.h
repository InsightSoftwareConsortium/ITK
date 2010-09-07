/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToMeshFilter_h
#define __itkImageToMeshFilter_h

#include "itkMeshSource.h"

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
template< class TInputImage, class TOutputMesh >
class ITK_EXPORT ImageToMeshFilter:public MeshSource< TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef ImageToMeshFilter          Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToMeshFilter, MeshSource);

  /** Create a valid output. */
  DataObject::Pointer  MakeOutput(unsigned int idx);

  /** Some Image related typedefs. */
  typedef   TInputImage                           InputImageType;
  typedef   typename InputImageType::Pointer      InputImagePointer;
  typedef   typename InputImageType::ConstPointer InputImageConstPointer;
  typedef   typename InputImageType::RegionType   InputImageRegionType;
  typedef   typename InputImageType::PixelType    InputImagePixelType;

  /** Some Mesh related typedefs. */
  typedef   TOutputMesh                      OutputMeshType;
  typedef   typename OutputMeshType::Pointer OutputMeshPointer;

  /** Set the input image of this process object.  */
  void SetInput(unsigned int idx, const InputImageType *input);

  /** Get the input image of this process object.  */
  const InputImageType * GetInput(unsigned int idx);

  /** Get the output Mesh of this process object.  */
  OutputMeshType * GetOutput(void);

  /** Prepare the output */
  void GenerateOutputInformation(void);

protected:
  ImageToMeshFilter();
  ~ImageToMeshFilter();
private:
  ImageToMeshFilter(const ImageToMeshFilter &); //purposely not implemented
  void operator=(const ImageToMeshFilter &);    //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToMeshFilter.txx"
#endif

#endif
