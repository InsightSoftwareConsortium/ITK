/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToParametricSpaceFilter_h
#define __itkImageToParametricSpaceFilter_h

#include "itkImageToMeshFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/** \class ImageToParametricSpaceFilter
 * \brief Generate a mesh of parametric space from input images.
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
  /** Standard class typedefs. */
  typedef ImageToParametricSpaceFilter  Self;
  typedef  ProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToParametricSpaceFilter, ProcessObject);

  /** Some typedefs associated with the input images. */
  typedef TInputImage                               InputImageType;
  typedef typename InputImageType::ConstPointer     InputImageConstPointer;
  typedef typename InputImageType::RegionType       InputImageRegionType; 
  typedef typename InputImageType::PixelType        InputImagePixelType; 
  typedef ImageRegionConstIteratorWithIndex<InputImageType> 
  InputImageIterator;

  /** Some typedefs associated with the output mesh. */
  typedef TOutputMesh OutputMeshType;
  typedef typename OutputMeshType::PointType        PointType;
  typedef typename OutputMeshType::Pointer          OutputMeshPointer;
  typedef typename OutputMeshType::PointsContainer  PointsContainer; 
  typedef typename OutputMeshType::PointIdentifier  PointIdentifier; 
  typedef typename PointsContainer::Pointer         PointsContainerPointer;
  typedef typename PointsContainer::Iterator        PointsContainerIterator;
  typedef typename OutputMeshType::PointDataContainer PointDataContainer; 
  typedef typename PointDataContainer::Pointer      PointDataContainerPointer;
  typedef typename PointDataContainer::Iterator     PointDataContainerIterator;

  /** The dimension of the output mesh. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TOutputMesh::PointDimension);

  /** Some typedefs associated with the output mesh. */
  void GenerateData(void);

  /** Prepare the output. */
  void GenerateOutputInformation(void);

  /** Select if the indices of input image pixels will be 
   * stored as data at each one of the mesh points.
   * That assumes that the type of PointData in the output
   * mesh is capable of accepting an itk::Index through
   * an operator=(). Default value = true */
  itkSetMacro( ComputeIndices, bool );

protected:
  ImageToParametricSpaceFilter();
  ~ImageToParametricSpaceFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;
 
private:
  ImageToParametricSpaceFilter(const ImageToParametricSpaceFilter&); //purposely not implemented
  void operator=(const ImageToParametricSpaceFilter&); //purposely not implemented

  /** This variable defines if the indices of input image pixels 
   * will be stored as Data at each one of the mesh points. */
  bool      m_ComputeIndices;
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToParametricSpaceFilter.txx"
#endif

#endif
