/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMaskToNarrowBandPointSetFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryMaskToNarrowBandPointSetFilter_h
#define __itkBinaryMaskToNarrowBandPointSetFilter_h

#include "itkImageToMeshFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkReinitializeLevelSetImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

namespace itk
{

/** \class BinaryMaskToNarrowBandPointSetFilter
 * \brief Generate a PointSet containing the narrow band around the edges of a
 * input binary image.
 *
 * BinaryMaskToNarrowBandPointSetFilter takes a binary image as input
 * and generates a PointSet as output. The point set contains
 * points around the contours of the binary mask in the image.
 * The pixel values of the point set are obtained as the distances
 * from the point to the edge of the binary mask.
 * 
 * This filter is intended to be used for initializing the process
 * of NarrowBand-to-Image Registration.
 *
 * The filter is templated over the input image type and the 
 * output mesh type. The only restriction is that the dimension 
 * of points in the mesh should be equal to the input image dimension.
 * The PixelType in the mesh should be capable to represent distance
 * values.
 * 
 * \sa ReinitializeImageFilter
 * \sa PointSetToImageRegistrationMethod
 * 
 * \ingroup ImageFilters  MeshFilters
 */
template <class TInputImage, class TOutputMesh>
class ITK_EXPORT BinaryMaskToNarrowBandPointSetFilter : 
    public ImageToMeshFilter<TInputImage,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef BinaryMaskToNarrowBandPointSetFilter  Self;
  typedef  ProcessObject                Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMaskToNarrowBandPointSetFilter, ImageToMeshFilter);

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
  typedef typename OutputMeshType::ConstPointer     OutputMeshConstPointer;
  typedef typename OutputMeshType::PointsContainer  PointsContainer; 
  typedef typename OutputMeshType::PointIdentifier  PointIdentifier; 
  typedef typename PointsContainer::Pointer         PointsContainerPointer;
  typedef typename PointsContainer::Iterator        PointsContainerIterator;
  typedef typename OutputMeshType::PointDataContainer PointDataContainer; 
  typedef typename PointDataContainer::Pointer      PointDataContainerPointer;
  typedef typename PointDataContainer::Iterator     PointDataContainerIterator;


  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);


  /** Float image type to be used by the ReinitializeLevelSet image filter */
  typedef itk::Image< float, 
                itkGetStaticConstMacro(ImageDimension) >   RealImageType;

  /** The ReinitializeLevelSet filter is used to evaluate the distance from
      every pixel to the border of the binary mask. It uses internally a 
      FastMarching filter for propagating a from from the edges of the binary
      mask.  */
  typedef typename itk::ReinitializeLevelSetImageFilter< 
                                            RealImageType >
                                                    DistanceFilterType;
 
  typedef typename DistanceFilterType::Pointer                  DistanceFilterPointer; 
  typedef typename DistanceFilterType::NodeContainerPointer     NodeContainerPointer;
  typedef typename DistanceFilterType::NodeContainer            NodeContainer;
  typedef typename NodeContainer::Element                       NodeType;



  /** The ReinitializeLevelSetImageFilter expect the input to be binary
      withing the range [-0.5:0.5]. This filte will scale the input to
      fit in this range. */
  typedef itk::RescaleIntensityImageFilter< 
                                        InputImageType, 
                                        RealImageType > RescaleFilterType;

  typedef typename RescaleFilterType::Pointer         RescaleFilterPointer;

  /** The dimension of the output mesh. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TOutputMesh::PointDimension);

  /** Some typedefs associated with the output mesh. */
  void GenerateData(void);

  /** Some typedefs associated with the output mesh. */
  void GenerateOutputInformation(void);

  /** accept the input image */
  void SetInput( const InputImageType * inputImage );

  /** Set/Get the width of the narrowband. This is the 
      maximum distance from the binary mask edges to
      the points in the narrow band. The full width of 
      the full narrow band will be double of this value. */
  itkSetMacro( BandWidth, float );
  itkGetMacro( BandWidth, float );

protected:
  BinaryMaskToNarrowBandPointSetFilter();
  ~BinaryMaskToNarrowBandPointSetFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;
 
private:
  BinaryMaskToNarrowBandPointSetFilter(const BinaryMaskToNarrowBandPointSetFilter&); //purposely not implemented
  void operator=(const BinaryMaskToNarrowBandPointSetFilter&); //purposely not implemented

  DistanceFilterPointer   m_DistanceFilter;
  RescaleFilterPointer    m_RescaleFilter;

  float                   m_BandWidth;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMaskToNarrowBandPointSetFilter.txx"
#endif

#endif
