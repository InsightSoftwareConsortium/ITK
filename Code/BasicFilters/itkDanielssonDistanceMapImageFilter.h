/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDanielssonDistanceMapImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDanielssonDistanceMapImageFilter_h
#define __itkDanielssonDistanceMapImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>

namespace itk
{

/** \class DanielssonDistanceMapImageFilter
*
* This class is parametrized over the type of the input image
* and the type of the output image.
*
* This filter computes the distance map of the input image 
* as an approximation with pixel accuracy to the Euclidean distance.
*
* The input is assumed to contain numeric codes defining objects.
* The filter will produce as output the following images:
*
* - A voronoi partition using the same numeric codes as the input.
* - A distance map with the approximation to the euclidean distance.
*   from a particular pixel to the nearest object to this pixel
*   in the input image.
* - A vector map containing the component of the vector relating
*   the current pixel with the closest point of the closest object
*   to this pixel. Given that the components of the distance are
*   computed in "pixels", the vector is represented by an
*   itk::Offset.  That is, physical coordinates are not used.
*
* This filter is N-dimensional and known to be efficient
* in computational time.  The algorithm is the N-dimensional version
* of the 4SED algorithm given for two dimensions in:
* 
* Danielsson, Per-Erik.  Euclidean Distance Mapping.  Computer
* Graphics and Image Processing 14, 227-248 (1980).
*
* \ingroup ImageFeatureExtraction 
*
*/

template <class TInputImage,class TOutputImage>
class ITK_EXPORT DanielssonDistanceMapImageFilter :
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef DanielssonDistanceMapImageFilter    Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( DanielssonDistanceMapImageFilter, ImageToImageFilter );

  /** Type for input image. */
  typedef   TInputImage       InputImageType;

  /** Type for two of the three output images: the VoronoiMap and the
   * DistanceMap.  */
  typedef   TOutputImage      OutputImageType;

  /** Type for the region of the input image. */
  typedef typename InputImageType::RegionType   RegionType;

  /** Type for the index of the input image. */
  typedef typename RegionType::IndexType  IndexType;

  /** Type for the index of the input image. */
  typedef typename InputImageType::OffsetType  OffsetType;

  /** Type for the size of the input image. */
  typedef typename RegionType::SizeType   SizeType;
  
  /** The dimension of the input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Pointer Type for the vector distance image */
  typedef Image< OffsetType,
                 itkGetStaticConstMacro(InputImageDimension)> VectorImageType;

  /** Pointer Type for input image. */
  typedef typename InputImageType::ConstPointer InputImagePointer;

  /** Pointer Type for the output image. */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Pointer Type for the vector distance image. */
  typedef typename VectorImageType::Pointer VectorImagePointer;

  /** Set if the distance should be squared. */
  itkSetMacro( SquaredDistance, bool );

  /** Get the distance squared. */
  itkGetConstReferenceMacro( SquaredDistance, bool );

  /** Set On/Off if the distance is squared. */
  itkBooleanMacro( SquaredDistance );

  /** Set if the input is binary. If this variable is set, each
   * nonzero pixel in the input image will be given a unique numeric
   * code to be used by the Voronoi partition.  If the image is binary
   * but you are not interested in the Voronoi regions of the
   * different nonzero pixels, then you need not set this.  */
  itkSetMacro( InputIsBinary, bool );

  /** Get if the input is binary.  See SetInputIsBinary(). */
  itkGetConstReferenceMacro( InputIsBinary, bool );

  /** Set On/Off if the input is binary.  See SetInputIsBinary(). */
  itkBooleanMacro( InputIsBinary );

  /** Get Voronoi Map
   * This map shows for each pixel what object is closest to it. 
   * Each object should be labeled by a number (larger than 0), 
   * so the map has a value for each pixel corresponding to the label 
   * of the closest object.  */
  OutputImageType * GetVoronoiMap(void);

  /** Get Distance map image.  The distance map is shown as a gray
   * value image depending on the pixel type of the output image.
   * Regarding the source image, background should be dark (gray value
   * = 0) and object should have a gray value larger than 0.  The
   * minimal distance is calculated on the object frontier, and the
   * output image gives for each pixel its minimal distance from the
   * object (if there is more than one object the closest object is
   * considered). */
  OutputImageType * GetDistanceMap(void);

  /** Get vector field of distances. */
  VectorImageType * GetVectorDistanceMap(void);

protected:
  DanielssonDistanceMapImageFilter();
  virtual ~DanielssonDistanceMapImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Compute Danielsson distance map and Voronoi Map. */
  void GenerateData();  

  /** Prepare data. */
  void PrepareData();  

  /**  Compute Voronoi Map. */
  void ComputeVoronoiMap();  

  /** Update distance map locally.  Used by GenerateData(). */
  void UpdateLocalDistance(VectorImageType*,
                           const IndexType&,
                           const OffsetType&);

private:   
  DanielssonDistanceMapImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool                  m_SquaredDistance;
  bool                  m_InputIsBinary;

}; // end of DanielssonDistanceMapImageFilter class

} //end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDanielssonDistanceMapImageFilter.txx"
#endif

#endif
