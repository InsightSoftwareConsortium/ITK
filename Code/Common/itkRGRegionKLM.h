/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGRegionKLM.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRGRegionKLM_h
#define _itkRGRegionKLM_h

#include "itkObject.h"
#include "itkRGRegion.h"
//#include "itkRGBorderKLM.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class RGRegionKLM
 * \brief Base class for RGRegionKLM object
 * 
 * itkRGRegionKLM is the base class for the RGRegionKLM objects. It provides
 * the basic function definitons that are inherent to a RGRegionKLM objects.
 * It is templated over the type of input and output image.

 * This object supports data handling of multiband images. The object
 * accepts images in vector format, where each pixel is a vector and each 
 * element of the vector corresponds to an entry from 1 particular band of
 * a multiband dataset. 

 * We expect the user to provide the input to the routine in vector format. 
 * A single band image is treated as a vector image with a single element 
 * for every vector.
 * 
 * Data structures for a regions
 * =============================
 * A region is defined as a closed area in the image that is surrounded
 * by a list of borders objects (see itkRGBorderKLM class).  
 *
 * Shown below is an initial 4x3 grid size partition of a 8x9
 * image. The initial region blocks are labeled in hexadecimal, and
 * the region borders are shown as E = border pixel, and C =
 * non-border pixel. Note that the border pixel grid is one pixel
 * larger than the region grid. The border pixel grid can be
 * considered to lie interspersed within the region pixel grid with
 * four border pixels surrounding each interior region pixel.
 *
 * \begin{center}
 * Initial regions of a 8 by 9 image with a 4 by 3 grid partition. \\
 * \begin{tabular}{|c|c|c|c|c|c|c|c|c|}
 * \hline
 *   1 & 1 & 1 & 2 & 2 & 2 & 3 & 3 & 3 \\ \hline
 *   1 & 1 & 1 & 2 & 2 & 2 & 3 & 3 & 3 \\ \hline
 *   4 & 4 & 4 & 5 & 5 & 5 & 6 & 6 & 6 \\ \hline
 *   4 & 4 & 4 & 5 & 5 & 5 & 6 & 6 & 6 \\ \hline
 *   7 & 7 & 7 & 8 & 8 & 8 & 9 & 9 & 9 \\ \hline
 *   7 & 7 & 7 & 8 & 8 & 8 & 9 & 9 & 9 \\ \hline
 *   a & a & a & b & b & b & c & c & c \\ \hline
 *   a & a & a & b & b & b & c & c & c \\ \hline
 *  \end{tabular}
 *  \end{center}
 *
 * \begin{center}
 * Region borders are shown as ``E''. \\
 * \begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}
 * \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *   E & E & E & E & E & E & E & E & E & E \\ \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *   E & E & E & E & E & E & E & E & E & E \\ \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *   E & E & E & E & E & E & E & E & E & E \\ \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *   C & C & C & E & C & C & E & C & C & C \\ \hline
 *  \end{tabular}
 *  \end{center}
 *
 */
template <class TInputImage, class TOutputImage>
class RGBorderKLM;

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RGRegionKLM : public RGRegion<TInputImage,TOutputImage>
{
 private:
  /**
   * Type definition for an double vector.
   */
  typedef vnl_matrix<double> VecDblType;

public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGRegionKLM   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef RGRegion<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RGRegionKLM,RGRegion);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /**
   * Type definition for the input image pixel vector type.
   */
  typedef typename TInputImage::PixelType::VectorType InputImageVectorType;

  /**
   * Type definition for the image iterators to be used.
   */
  typedef
    ImageRegionIterator< InputImagePixelType, TInputImage::ImageDimension>  
      InputImageIterator;

  /**
   * Type definition for vector container that stores the borders
   * associated with a current region.
   */             
  typedef 
    std::vector< RGBorderKLM<TInputImage,TOutputImage>* > 
      RegionBorderVecType;

  /**
   * Type definition for the region border vector iterators to be used.
   */
  typedef typename RegionBorderVecType::iterator RegionBorderVecIterator;

  /**
   * Set the region with parameter values
   * defining the region.
   */
  void SetRegion(VecDblType regionMeanIntensity, 
                 unsigned int regionArea,
                 unsigned int label);
 

  /**
   * Set the border associated with a region.
   */
  void SetRegionBorder(RGBorderKLM<TInputImage,TOutputImage> *pNewRegionBorder);

  /**
   * get the first border associated with a region.
   */
  RGBorderKLM<TInputImage,TOutputImage> *GetFirstRegionBorder();

  /**
   * Delete a region border
   */
  void DeleteRegionBorder(RGBorderKLM<TInputImage,
                                      TOutputImage> *pBorderCandidate);
  /**
   * Insert a region border
   */
  void InsertRegionBorder(RegionBorderVecIterator it,
                          RGBorderKLM<TInputImage,
                                      TOutputImage> *pBorderCandidate);

  /**
   * Reorder the region borders given a candidate border after region 
   * merging
   */
  void ReorderRegionBorders(RGBorderKLM<TInputImage,
                                        TOutputImage> *pBorderCandidate);

  /**
   * Get a head pointer to the vector containter storing the borders associated
   * with a region
   */
  RegionBorderVecIterator GetRegionBorderItBegin();

  /**
   * Get a tail pointer to the vector containter storing the borders associated
   * with a region
   */
  RegionBorderVecIterator GetRegionBorderItEnd();

  /**
   * Recalculate the lambda values for all the borders defining the region
   * and resort the entire border list in decending order of the lambda values
   */
  void UpdateRegionBorderLambda();

  /**
   * Function that allows printing of the region parameters using std::cout
   */
  void PrintRegionInfo();

  /**
   * Constructor
   */
  RGRegionKLM();

  /**
   * Destructor
   */
  ~RGRegionKLM();

  /**
   * Copy constructor
   */
  RGRegionKLM(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

protected:
  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

private:

  RegionBorderVecType    m_RegionBorderVec;

}; // class RGRegion


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGRegionKLM.txx"
#endif



#endif

