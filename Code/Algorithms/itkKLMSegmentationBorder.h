/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.h
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
#ifndef _itkKLMSegmentationBorder_h
#define _itkKLMSegmentationBorder_h

#include "itkObject.h"
#include "itkSegmentationBorder.h"
#include "itkKLMSegmentationRegion.h"

namespace itk
{

/** \class DynamicBorderArrayKLM
 * \brief  Object maintaining a reference to a list of border associated 
 * with a region.
 *
 * This is a tiny class similar to smart pointers that maintains a reference
 * to a list of borders pointed by a region. 
 * 
 */

template <class TBorder> 
class DynamicBorderArrayKLM
{
public:
  /**
   * Greater than operators defined to work with both static objects
   * or pointer to objects.
   */
  bool operator> (const DynamicBorderArrayKLM<TBorder>& rhs) const
  {
    return(m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda());
  }

  bool operator> (const DynamicBorderArrayKLM<TBorder>* rhs) const
  {
    return(m_Pointer->GetLambda() > rhs.m_Pointer->GetLambda());
  }

  TBorder *m_Pointer;

};

/** \class KLMSegmentationBorder
 * \brief Base class for KLMSegmentationBorder object
 * 
 * itkKLMSegmentationBorder is the base class for the KLMSegmentationBorder objects. It provides
 * the basic function definitons that are inherent to a KLMSegmentationBorder objects.
 *
 * This class implements the border object that is used in particular with 
 * the KLM algorithm (see also itkRegionGrowImageFilterKLM). The border is defined by 
 * the adjacency of two regions. The parameter Lambda acertains the importance
 * of the border in defining the regions. The higher the values of lambda
 * the more dominant is its presence in the a region. In case of removal
 * of a border during the region growing process the one with least lambda
 * value is eliminated.
 * 
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT KLMSegmentationBorder : public SegmentationBorder<TInputImage,TOutputImage>
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
  typedef KLMSegmentationBorder   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef SegmentationBorder<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(KLMSegmentationBorder,SegmentationBorder);

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
   * Set the region 1 associated with the border
   */
  void SetRegion1(KLMSegmentationRegion<TInputImage,TOutputImage> *Region1);

  /**
   * Get the region 1 associated with the border
   */
  KLMSegmentationRegion<TInputImage,TOutputImage> *GetRegion1();

  /**
   * Set the region 2 associated with the border
   */
  void SetRegion2(KLMSegmentationRegion<TInputImage,TOutputImage> *Region2);

  /**
   * Get the region 2 associated with the border
   */
  KLMSegmentationRegion<TInputImage,TOutputImage> *GetRegion2();

  /**
   * Set the lamba parameter associate with the borders
   * in the KLM algorithm
   */
  itkSetMacro(Lambda, double);

  /**
   * Get the lamba parameter associated with the borders
   * in the KLM algorithm
   */
  itkGetMacro(Lambda, double);

  /**
   * Evaluate the lambda for a given border
   */
  void EvaluateLambda();

  /**
   * Print the data associated with each border 
   */
  void PrintBorderInfo();

  /**
   * Constructor
   */
  KLMSegmentationBorder();

  /**
   * Destructor
   */
  ~KLMSegmentationBorder();

  /**
   * Greater than operators defined to work with both static objects
   * or pointer to objects.
   */
  bool operator> (const KLMSegmentationBorder<TInputImage,TOutputImage>& rhs) const
  {
    return(m_Lambda > rhs.m_Lambda);
  }

  bool operator> (const KLMSegmentationBorder<TInputImage,TOutputImage>* rhs) const
  {
    return(m_Lambda > rhs->m_Lambda);
  }

protected:
  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

private:

  double                                m_Lambda;
  KLMSegmentationRegion<TInputImage,TOutputImage> *m_Region1;
  KLMSegmentationRegion<TInputImage,TOutputImage> *m_Region2;


}; // class KLMSegmentationBorder


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKLMSegmentationBorder.txx"
#endif



#endif
