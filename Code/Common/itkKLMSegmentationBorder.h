/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKLMSegmentationBorder_h
#define _itkKLMSegmentationBorder_h

#include "itkObject.h"
#include "itkSegmentationBorder.h"

#include "itkKLMSegmentationRegion.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_matrix.h"
namespace itk
{

/** \class KLMDynamicBorderArray
 * \brief  Object maintaining a reference to a list of border associated 
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
   * or pointer to objects. */
  bool operator> (const KLMDynamicBorderArray<TBorder>& rhs) const
    {  
    if( m_Pointer->GetLambda() == rhs.m_Pointer->GetLambda() ) 
      { 

      unsigned int lhsArea = ( m_Pointer->GetRegion1()->GetRegionArea() + 
        m_Pointer->GetRegion2()->GetRegionArea() );

      unsigned int rhsArea = ( rhs.m_Pointer->GetRegion1()->GetRegionArea() + 
        rhs.m_Pointer->GetRegion2()->GetRegionArea() );

      // Compare the total areas of the two neighbors
      if( lhsArea == rhsArea )
        {
        return ( m_Pointer > rhs.m_Pointer );
        }
      else
        {
        return ( lhsArea > rhsArea );
        }
      }
    return(m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda() ); 
    }

  bool operator> (const KLMDynamicBorderArray<TBorder>* rhs) const
    { 
    if( m_Pointer->GetLambda() == rhs.m_Pointer->GetLambda() ) 
      { 
      return ( m_Pointer > rhs.m_Pointer );
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
 * defined by the adjacency of two regions. The parameter Lambda acertains
 * the importance of the border in defining the regions. The higher the
 * values of lambda the more dominant is its presence in the a region. In
 * case of removal of a border during the region growing process the one with
 * least lambda value is eliminated.
 *
 * \ingroup RegionGrowingSegmentation 
 */

class KLMSegmentationRegion;

class ITKCommon_EXPORT KLMSegmentationBorder : public SegmentationBorder
{
private:
  /** Type definition for an double vector. */
  typedef vnl_matrix<double> VecDblType;

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

  /** Set the lamba parameter associate with the borders
   * in the KLM algorithm */
  itkSetMacro(Lambda, double);

  /** Get the lamba parameter associated with the borders
   * in the KLM algorithm */
  itkGetMacro(Lambda, double);

  /** Evaluate the lambda for a given border. */
  void EvaluateLambda();

  /** Print the data associated with each border. */
  void PrintBorderInfo();

  /** Greater than operators defined to work with both static objects
   * or pointer to objects. */
  bool operator> (const KLMSegmentationBorder& rhs) const
    { return( m_Lambda > rhs.m_Lambda ); }
//    { if( m_Lambda == rhs.m_Lambda) { return ( this > &rhs ); }
      

  bool operator> (const KLMSegmentationBorder* rhs) const
    { return( (m_Lambda > rhs->m_Lambda) ); }
//    { if( m_Lambda == rhs->m_Lambda) { return ( this > rhs ); }
      

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

  double                                m_Lambda;
  KLMSegmentationRegion *m_Region1;
  KLMSegmentationRegion *m_Region2;


}; // class KLMSegmentationBorder


} // namespace itk


#endif
