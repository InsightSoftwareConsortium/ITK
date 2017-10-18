/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkKLMSegmentationBorder_h
#define itkKLMSegmentationBorder_h

#include "itkSegmentationBorder.h"
#include "itkKLMSegmentationRegion.h"
#include "itkMacro.h"
#include "ITKKLMRegionGrowingExport.h"

#include "itkMath.h"
#include "vnl/vnl_vector.h"
#include "itkMath.h"

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
 * \ingroup ITKKLMRegionGrowing
 */

template< typename TBorder >
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
  bool operator>(const KLMDynamicBorderArray< TBorder > & rhs) const
  {
    if ( Math::ExactlyEquals(m_Pointer->GetLambda(), rhs.m_Pointer->GetLambda()) )
      {
      if ( m_Pointer->GetLambda() < 0 )
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
        double v1 = std::max(
          static_cast< double >( m_Pointer->GetRegion1()->GetRegionBorderSize() ),
          static_cast< double >( m_Pointer->GetRegion2()->GetRegionBorderSize() ) );

        double v2 = std::max(
          static_cast< double >( rhs.m_Pointer->GetRegion1()->GetRegionBorderSize() ),
          static_cast< double >( rhs.m_Pointer->GetRegion2()->GetRegionBorderSize() ) );

        return ( v1 > v2 );
        }
      }
    return ( m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda() );
  }

  bool operator>(const KLMDynamicBorderArray< TBorder > *rhs) const
  {
    if ( m_Pointer->GetLambda() == rhs->m_Pointer->GetLambda() )
      {
      if ( m_Pointer->GetLambda() < 0 )
        {
        return ( m_Pointer > rhs->m_Pointer );
        }
      else
        {
        // The purpose of this comparison is to not let any one region
        // get more borders than another region.  In the degenerate
        // case of an image where the Lambdas are always equal to some
        // constant C, allowing a single region to be repeatedly
        // merged so that it gains many borders will result in
        // pathologically slow behavior.
        double v1 = std::max(
          static_cast< double >( m_Pointer->GetRegion1()->GetRegionBorderSize() ),
          static_cast< double >( m_Pointer->GetRegion2()->GetRegionBorderSize() ) );

        double v2 = std::max(
          static_cast< double >( rhs->m_Pointer->GetRegion1()->GetRegionBorderSize() ),
          static_cast< double >( rhs->m_Pointer->GetRegion2()->GetRegionBorderSize() ) );

        return ( v1 > v2 );
        }
      }
    return ( m_Pointer->GetLambda() > rhs->m_Pointer->GetLambda() );
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
 * \ingroup ITKKLMRegionGrowing
 */

// Forward reference because of circular dependencies
class ITK_FORWARD_EXPORT KLMSegmentationRegion;

class ITKKLMRegionGrowing_EXPORT KLMSegmentationBorder:public SegmentationBorder
{
public:
  /** Standard class typedefs. */
  typedef KLMSegmentationBorder      Self;
  typedef SegmentationBorder         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KLMSegmentationBorder, SegmentationBorder);

  /** Set the region 1 associated with the border */
  void SetRegion1(KLMSegmentationRegion *Region1);

  /** Get the region 1 associated with the border. */
  KLMSegmentationRegion * GetRegion1();

  /** Set the region 2 associated with the border. */
  void SetRegion2(KLMSegmentationRegion *Region2);

  /** Get the region 2 associated with the border. */
  KLMSegmentationRegion * GetRegion2();

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
  ~KLMSegmentationBorder() ITK_OVERRIDE;

  /** Print self identity */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KLMSegmentationBorder);

  double                 m_Lambda;
  KLMSegmentationRegion *m_Region1;
  KLMSegmentationRegion *m_Region2;
};
} // end namespace itk

#endif
