/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageFilterRegionCopier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExtractImageFilterRegionCopier_h
#define __itkExtractImageFilterRegionCopier_h

#include "itkImageToImageFilterDetail.h"

namespace itk
{

namespace ImageToImageFilterDetail
{
 
  /**
   *  Call the base class version: ImageToImageFilterDefaultCopyRegion
   */
  template <unsigned int D1, unsigned int D2>
  void ExtractImageFilterCopyRegion(const typename
                  BinaryUnsignedIntDispatch<D1, D2>::FirstEqualsSecondType &firstEqualsSecond,
                  ImageRegion<D1> &destRegion,
                  const ImageRegion<D2> &srcRegion,
                  const ImageRegion<D1> &totalInputExtractionRegion)
  {
    ImageToImageFilterDefaultCopyRegion<D1, D2>(firstEqualsSecond, destRegion, srcRegion);
  }

  

  /**
   *  Call the base class version: ImageToImageFilterDefaultCopyRegion
   */
  template <unsigned int D1, unsigned int D2>
  void ExtractImageFilterCopyRegion(const typename
                  BinaryUnsignedIntDispatch<D1, D2>::FirstLessThanSecondType &firstLessThanSecond,
                  ImageRegion<D1> &destRegion,
                  const ImageRegion<D2> &srcRegion,
                  const ImageRegion<D1> &totalInputExtractionRegion)
  {
    ImageToImageFilterDefaultCopyRegion<D1, D2>(firstLessThanSecond, destRegion, srcRegion);

  }


  /**
   * Copy an image region (start index and size) for the case where
   * the source region has a lesser dimension than the destination
   * region.  This version is "smart" in that it looks at what 
   * dimensions should be collapsed.  This information is passed in
   * the parameter totalInputExtractionRegion.  Any values within
   * totalInputExtractionRegion.Size that are zero correspond to the 
   * dimension to be collapsed.
   *
   * Note that the use of source and destination reflect where
   * where is information is coming from and going to.  When used
   * in the pipeline mechanism, the region requested by the output
   * of a filter is used to define the region required on the input.
   * In this case the output of the filter is the source and the
   * input of the filter is the destination.
   */
  template <unsigned int D1, unsigned int D2>
  void ExtractImageFilterCopyRegion(const typename
               BinaryUnsignedIntDispatch<D1, D2>::FirstGreaterThanSecondType &,
               ImageRegion<D1> &destRegion,
               const ImageRegion<D2> &srcRegion,
               const ImageRegion<D1> &totalInputExtractionRegion)
  {
    // Source dimension is less than the destination dimension, so look
    // at the m_TotalExtractionRegion and see what values in size are 0.
    // With these values, lock the destRegion.Index to the corresponding index
    unsigned int dim;
    Index<D1> destIndex;
    Size<D1>  destSize;
    const Index<D2> &srcIndex = srcRegion.GetIndex();
    const Size<D2> &srcSize = srcRegion.GetSize();
    int count = 0;
    for (dim=0; dim < D1; ++dim)
      {
      //for dimensions to be removed
      if (!totalInputExtractionRegion.GetSize()[dim])
        {
        destIndex[dim] = totalInputExtractionRegion.GetIndex()[dim];
        destSize[dim] = 1;
        }
      //for all other dimension
      else
        {
        destIndex[dim] = srcIndex[count];
        destSize[dim] = srcSize[count];
        count++;
        }
      }
    destRegion.SetIndex(destIndex);
    destRegion.SetSize(destSize);
  }



  /**
   *  ExtractImageFilterRegionCopier is a special variation on ImageRegionCopier.
   *  The difference in this version is when the D1 > D2.  In this case, the 
   *  output image has fewer dimension than the input image.  This only works correctly
   *  when totalInputExtractionRegion has been set.  totalInputExtractionRegion is 
   *  essentially the mapping from the srcRegion space to the DestRegionSpace.  
   *  The important values in totalInputExtractionRegion are when 
   *  totalInputExtractionRegion.Size is = 0 for one or more of the dimensions.
   *  These values correspond to the dimensions to collapse.  When
   *  totalInputExtractionRegion.Size[dim] = 0, then the index that we have to lock 
   *  destRegion.Index[dim] = totalInputExtractionRegion.Index[dim].
   *  
   *  The other two cases (D1 = D2, and D1 < D2) are identical to the implementation 
   *  in ImageToImageFilterDetail.
   *
   *  
   **/
  template <unsigned int D1, unsigned int D2>
  class ITK_EXPORT ExtractImageFilterRegionCopier
    : public ImageRegionCopier<D1, D2>
  {
  public:
    virtual void operator()(ImageRegion<D1> &destRegion,
                            const ImageRegion<D2> &srcRegion,
                            const ImageRegion<D1> &totalInputExtractionRegion) const
    {
      ExtractImageFilterCopyRegion<D1, D2>(BinaryUnsignedIntDispatch<D1, D2>::ComparisonType(),
                                          destRegion, srcRegion, totalInputExtractionRegion);
    }

    /** Duplicate the superclass method to avoid warnings. */
    virtual void operator() (ImageRegion<D1> &destRegion,
                            const ImageRegion<D2> &srcRegion) const
    {
      ImageRegionCopier<D1,D2>::operator()(destRegion, srcRegion);
    }
  };



} //end namespace ImageToImageFilterDetail
} //end namespace itk

#endif
