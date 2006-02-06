/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKLMSegmentationBorder_h
#define __itkKLMSegmentationBorder_h

#include "itkObject.h"
#include "itkSegmentationBorder.h"
#include "itkKLMSegmentationRegion.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"
#include "vnl/vnl_vector.h"

namespace itk
{

/** \class KLMDynamicBorderArray
 * \brief  Object maintaining a reference to a list of borders associated
 * with a region.
 *
 * This is a tiny class similar to smart pointers that maintains a reference
 * to a list of borders pointed by a region.
 *
 * \ingroup RegionGrowingSegmentation
 */
template <class TBorder>
class KLMDynamicBorderArray
{
public:
  /** Greater than operators defined to work with both static objects
   * or pointer to objects.  In the degenerate
   *  case of an image where all (or many) Lambda's are equal to some
   *  constant value, this operator will ensure that the future
   *  merged regions do not gain more borders than other regions,
   *  thus avoiding pathologically slow behavior.
   */
  bool operator> (const KLMDynamicBorderArray<TBorder>& rhs) const
    {
    if( m_Pointer->GetLambda() == rhs.m_Pointer->GetLambda() )
      {
      if( m_Pointer->GetLambda() < 0 )
        {
        return ( m_Pointer > rhs.m_Pointer );
        }
      else
        {
        // The purpose of this comparison is to not let any one region
        // get more borders than another region.  In the degenerate
        // case of an image where the Lambdas are always equal to some
        // constant C, allowing a single region to be repeatedly
        // merged so that it gains many borders will result in
        // pathologically slow behavior.
        unsigned int v1 = vnl_math_max(
          m_Pointer->GetRegion1()->GetRegionBorderSize(),
          m_Pointer->GetRegion2()->GetRegionBorderSize() );

        unsigned int v2 = vnl_math_max(
          rhs.m_Pointer->GetRegion1()->GetRegionBorderSize(),
          rhs.m_Pointer->GetRegion2()->GetRegionBorderSize() );

        return ( v1 > v2 );
        }
      }
    return(m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda() );
    }

  bool operator> (const KLMDynamicBorderArray<TBorder>* rhs) const
    {
    if( m_Pointer->GetLambda() == rhs.m_Pointer->GetLambda() )
      {
      if( m_Pointer->GetLambda() < 0 )
        {
        return ( m_Pointer > rhs.m_Pointer );
        }
      else
        {
        // The purpose of this comparison is to not let any one region
        // get more borders than another region.  In the degenerate
        // case of an image where the Lambdas are always equal to some
        // constant C, allowing a single region to be repeatedly
        // merged so that it gains many borders will result in
        // pathologically slow behavior.
        unsigned int v1 = vnl_math_max(
          m_Pointer->GetRegion1()->GetRegionBorderSize(),
          m_Pointer->GetRegion2()->GetRegionBorderSize() );

        unsigned int v2 = vnl_math_max(
          rhs.m_Pointer->GetRegion1()->GetRegionBorderSize(),
          rhs.m_Pointer->GetRegion2()->GetRegionBorderSize() );

        return ( v1 > v2 );
        }
      }
    return(m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda() );
    }

  TBorder *m_Pointer;
};

/** \class KLMSegmentationBorder
 * \brief Base class for KLMSegmentationBorder object
 *
 * itkKLMSegmentationBorder is the base class for the KLMSegmentationBorder
 * objects. It provides the basic function definitons that are inherent to a
 * KLMSegmentationBorder objects.
 *
 * This class implements the border object that is used in particular with
 * the KLM algorithm (see also KLMRegionGrowImageFilter). The border is
 * defined by the adjacency of two regions. The parameter Lambda ascertains
 * the importance of the border in defining the regions. The higher the
 * values of Lambda the more dominant is its presence in the a region. In
 * case of removal of a border during the region growing process the one with
 * least Lambda value is eliminated.
 *
 * \ingroup RegionGrowingSegmentation
 */

class KLMSegmentationRegion;

class ITKCommon_EXPORT KLMSegmentationBorder : public SegmentationBorder
{

public:
  /** Standard class typedefs. */
  typedef KLMSegmentationBorder   Self;
  typedef SegmentationBorder Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KLMSegmentationBorder,SegmentationBorder);

  /** Set the region 1 associated with the border */
  void SetRegion1(KLMSegmentationRegion *Region1);

  /** Get the region 1 associated with the border. */
  KLMSegmentationRegion *GetRegion1();

  /** Set the region 2 associated with the border. */
  void SetRegion2(KLMSegmentationRegion *Region2);

  /** Get the region 2 associated with the border. */
  KLMSegmentationRegion *GetRegion2();

  /** Set/Get the Lambda parameter associate with the borders
   * in the KLM algorithm */
  itkSetMacro(Lambda, double);
  itkGetConstReferenceMacro(Lambda, double);

  /** Evaluate the Lambda for a given border. */
  void EvaluateLambda();

  /** Print the data associated with each border. */
  void PrintBorderInfo();

protected:
  /** Constructor. */
  KLMSegmentationBorder();

  /** Destructor. */
  ~KLMSegmentationBorder();

  /** Print self identity */
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  KLMSegmentationBorder(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  double m_Lambda;
  KLMSegmentationRegion *m_Region1;
  KLMSegmentationRegion *m_Region2;

};


} // end namespace itk


#endif
