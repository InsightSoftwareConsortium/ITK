/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKLMSegmentationRegion_h
#define _itkKLMSegmentationRegion_h

#include "itkObject.h"
#include "itkKLMSegmentationBorder.h"
#include "itkSegmentationRegion.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class KLMSegmentationRegion
 * \brief Base class for KLMSegmentationRegion object
 * 
 * itkKLMSegmentationRegion is the base class for the KLMSegmentationRegion
 * objects. It provides the basic function definitons that are inherent to a
 * KLMSegmentationRegion objects.  It is templated over the type of input and
 * output image.

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
 * by a list of borders objects (see itkKLMSegmentationBorder class).  
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
 * \ingroup RegionGrowingSegmentation 
 */

class KLMSegmentationBorder;

class ITKCommon_EXPORT KLMSegmentationRegion : public SegmentationRegion
{
private:
  /** Type definition for an double vector. */
  typedef vnl_matrix<double> VectorOfDoubleType;

public:
  /** Standard class typedefs. */
  typedef KLMSegmentationRegion   Self;
  typedef SegmentationRegion Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KLMSegmentationRegion,SegmentationRegion);

  /** Type definition for vector container that stores the borders
   * associated with a current region. */             
  typedef std::vector< KLMSegmentationBorder* > 
      RegionBorderVectorType;

  /** Type definition for the region border vector iterators to be used. */
  typedef RegionBorderVectorType::iterator RegionBorderVectorIterator;

  /** Set the region with parameter values
   * defining the region. */
  void SetRegion(VectorOfDoubleType regionMeanIntensity, 
                 unsigned int regionArea,
                 unsigned int label);

  /** Set the border associated with a region. */
  void SetRegionBorder(KLMSegmentationBorder *pNewRegionBorder);

    /** Set the border associated with a region. */
  void SetRegionBorder3d(KLMSegmentationBorder *pNewRegionBorder);

  /** Get the first border associated with a region. */
  KLMSegmentationBorder *GetFirstRegionBorder();

  /** Delete a region border. */
  void DeleteRegionBorder(KLMSegmentationBorder *pBorderCandidate);
  /** Insert a region border. */
  void InsertRegionBorder(RegionBorderVectorIterator it,
                          KLMSegmentationBorder *pBorderCandidate);

  /** Delete all region borders */
  void DeleteAllRegionBorders();

  /** Reorder the region borders given a candidate border after region 
   * merging. */
  void ReorderRegionBorders(KLMSegmentationBorder *pBorderCandidate);

  /** Get a head pointer to the vector containter storing the borders
   * associated with a region. */
  RegionBorderVectorIterator GetRegionBorderItBegin();

  /** Get a tail pointer to the vector containter storing the borders
   * associated with a region. */
  RegionBorderVectorIterator GetRegionBorderItEnd();

  /** Recalculate the lambda values for all the borders defining the region
   * and resort the entire border list in decending order of the lambda
   * values. */
  void UpdateRegionBorderLambda();

  /** Function that allows printing of the region parameters using std::cout. */
  void PrintRegionInfo();

protected:
  KLMSegmentationRegion();
  ~KLMSegmentationRegion();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  KLMSegmentationRegion(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  RegionBorderVectorType    m_RegionBorderVector;

}; // class SegmentationRegion


} // namespace itk


#endif

