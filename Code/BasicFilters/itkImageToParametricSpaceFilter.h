/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilter.h
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
#ifndef __itkImageToParametricSpaceFilter_h
#define __itkImageToParametricSpaceFilter_h

#include "itkImageToMeshFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

/** \class ImageToParametricSpaceFilter
 * \brief 
 *
 * ImageToParametricSpaceFilter takes a three Images of equal dimension and 
 * size and generates from them a Mesh. 
 * 
 * The mesh contains one point for every pixel on the images. The 
 * coordinate of the point being equal to the gray level of the 
 * associated input pixels.
 *
 * This class is intended to produce the population of points that 
 * represent samples in a parametric space. In this particular case
 * the parameters are the gray levels of the input images
 *
 * The dimension of the mesh points should be equal to the number
 * of input images to this filter.
 *
 * \ingroup ImageFilters
 */
template <class TInputImage, class TOutputMesh>
class ITK_EXPORT ImageToParametricSpaceFilter : 
    public ImageToMeshFilter<TInputImage,TOutputMesh>
{
public:
  /**
   * Standard class typedefs.
   */
  typedef ImageToParametricSpaceFilter  Self;

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
  itkTypeMacro(ImageToParametricSpaceFilter, ProcessObject);


  /** 
   * Some typedefs associated with the input images
   */
  typedef TInputImage                               InputImageType;
  typedef typename InputImageType::Pointer          InputImagePointer;
  typedef typename InputImageType::RegionType       InputImageRegionType; 
  typedef typename InputImageType::PixelType        InputImagePixelType; 

  typedef ImageRegionIteratorWithIndex<InputImageType> 
                                                    InputImageIterator;

  /** 
   * Some typedefs associated with the output mesh
   */
  typedef TOutputMesh OutputMeshType;
  typedef typename OutputMeshType::PointType         PointType;
  typedef typename OutputMeshType::Pointer           OutputMeshPointer;
  typedef typename OutputMeshType::PointsContainer   PointsContainer; 
  typedef typename OutputMeshType::PointIdentifier   PointIdentifier; 
  typedef typename PointsContainer::Pointer          PointsContainerPointer;
  typedef typename PointsContainer::Iterator         PointsContainerIterator;

  typedef typename OutputMeshType::PointDataContainer   PointDataContainer; 
  typedef typename PointDataContainer::Pointer          PointDataContainerPointer;
  typedef typename PointDataContainer::Iterator         PointDataContainerIterator;

  enum { PointDimension = TOutputMesh::PointDimension };

  /** 
   * Some typedefs associated with the output mesh
   */
  void GenerateData(void);

  /** 
   * Prepare the output
   */
  void GenerateOutputInformation(void);

  /** 
   * Select if the indices of input image pixels will be 
   * stored as data at each one of the mesh points.
   * That assumes that the type of PointData in the output
   * mesh is capable of accepting an itk::Index through
   * an operator=().
   *
   * Default value = true
   */
  itkSetMacro( ComputeIndices, bool );


protected:
  ImageToParametricSpaceFilter();
  ~ImageToParametricSpaceFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

 
private:
  ImageToParametricSpaceFilter(const ImageToParametricSpaceFilter&); //purposely not implemented
  void operator=(const ImageToParametricSpaceFilter&); //purposely not implemented

  /**
   * This variable defines if the indices of input image pixels 
   * will be stored as Data at each one of the mesh points.
   */
  bool      m_ComputeIndices;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToParametricSpaceFilter.txx"
#endif

#endif
