/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDanielssonDistanceMapImageFilter.h
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
#ifndef __itkDanielssonDistanceMapImageFilter_h
#define __itkDanielssonDistanceMapImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/** \class DanielssonDistanceMapImageFilter
*
* This class is parametrized over the type of the input image
* and the type of the output image.
* This class returns the distance map of the input image 
* and the Voronoi map.
*
* 
* \ingroup ImageFilters
*
*/

template <class TInputImage,class TOutputImage>
class ITK_EXPORT DanielssonDistanceMapImageFilter :
                 public ImageToImageFilter<TInputImage,TOutputImage>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef   DanielssonDistanceMapImageFilter 	  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  
  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;

  
  /**
   * Const Smart pointer typedef support
   */
  typedef SmartPointer<const Self> ConstPointer;

   /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( DanielssonDistanceMapImageFilter, ImageBase);


  /**
   * Method for creation through the object factory
   */
  itkNewMacro(Self);

 
  /**
   * Type for input image
   */
  typedef   TInputImage       InputImageType;


  /**
   * Type for the output image
   */
  typedef   TOutputImage      OutputImageType;



  /**
   * Type for the region of the input image
   */
  typedef typename InputImageType::RegionType   RegionType;



  /**
   * Type for the index of the input image
   */
  typedef typename RegionType::IndexType  IndexType;


  /**
   * Type for the index of the input image
   */
  typedef typename InputImageType::OffsetType  OffsetType;


  /**
   * Type for the size of the input image
   */
  typedef typename RegionType::SizeType   SizeType;

  
  /**
   * The dimension of the input image.
   */
  enum { InputImageDimension = InputImageType::ImageDimension };

  /**
   * Pointer Type for the vector distance image
   */
  typedef Image< OffsetType, InputImageDimension > VectorImageType;


  /**
   * Pointer Type for input image
   */
  typedef typename InputImageType::Pointer InputImagePointer;


  /**
   * Pointer Type for the output image
   */
  typedef typename OutputImageType::Pointer OutputImagePointer;


  /**
   * Pointer Type for the vector distance image
   */
  typedef typename VectorImageType::Pointer VectorImagePointer;


  /**
   * Set if the distance should be squared
   */
   itkSetMacro( SquaredDistance, bool );


  /**
   * Get the distance squared
   */
   itkGetConstReferenceMacro( SquaredDistance, bool );


  /**
   * Set if the input is binary
   */
   itkSetMacro( InputIsBinary, bool );


  /**
   * Get if the input is binary
   */
   itkGetConstReferenceMacro( InputIsBinary, bool );




  /**
   * Compute Danielsson distance map and Voronoi Map
   */
  void GenerateData();  


  /**
   * Get Voronoi Map
   * This Map shows for each pixel what object is closest to it. 
   * Each object should be labeled by a number (larger than 0), 
   * so the map has a value for each pixel corresponding to the label 
   * of the closest object. 
   */
  OutputImagePointer GetVoronoiMap();


  /**
   * Get Distance map image
   * The distance map is shown as a gray value image depending on 
   * the pixel type of the output image.
   * Regarding the source image, background should be dark (gray value = 0) 
   * and object should have a gray value larger than 0. 
   * The minimal distance is calculated on the object frontier, 
   * and the output image gives for each pixel its minimal distance from the object 
   * (if there is more than one object the closest object is considered).
   */
  OutputImagePointer GetDistanceMap();


  /**
   * Get vector field of distances
   */
  VectorImagePointer GetVectorDistanceMap();


  /**
   * Connect input image to the filter
   */
   void SetInputImage( InputImageType * InputImage );

protected:

  DanielssonDistanceMapImageFilter();
  virtual ~DanielssonDistanceMapImageFilter() {};
  DanielssonDistanceMapImageFilter(const Self&) {}
  void operator=(const Self&) {}


  /**
   * Prepare data
   */
  void PrepareData();  


  /**
   *  Compute Voronoi Map
   */
  void ComputeVoronoiMap();  


  /**
   * Update distance map locally
   */
  void UpdateLocalDistance(VectorImageType*,
                           const IndexType&,
                           const OffsetType&);

private:   

  bool                  m_SquaredDistance;
  bool                  m_InputIsBinary;


  /** \class DIS SquaredEuclideanDistanceCalculator
  *
  *  This is a helper class that computes the squared distance
  */
  template <unsigned int VDimension>  
  class SquaredDistance
  {
  }; // end of SquaredDistance class

}; // end of DanielssonDistanceMapImageFilter class

} //end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDanielssonDistanceMapImageFilter.txx"
#endif

#endif
