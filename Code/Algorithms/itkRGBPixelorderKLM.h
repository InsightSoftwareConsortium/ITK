/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixelorderKLM.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRGBPixelorderKLM_h
#define _itkRGBPixelorderKLM_h

#include "itkObject.h"
#include "itkRGBPixelorder.h"
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

/** \class RGBPixelorderKLM
 * \brief Base class for RGBPixelorderKLM object
 * 
 * itkRGBPixelorderKLM is the base class for the RGBPixelorderKLM objects. It provides
 * the basic function definitons that are inherent to a RGBPixelorderKLM objects.
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
class ITK_EXPORT RGBPixelorderKLM : public RGBPixelorder<TInputImage,TOutputImage>
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
  typedef RGBPixelorderKLM   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef RGBPixelorder<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RGBPixelorderKLM,RGBPixelorder);

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
  RGBPixelorderKLM();

  /**
   * Destructor
   */
  ~RGBPixelorderKLM();

  /**
   * Greater than operators defined to work with both static objects
   * or pointer to objects.
   */
  bool operator> (const RGBPixelorderKLM<TInputImage,TOutputImage>& rhs) const
  {
    return(m_Lambda > rhs.m_Lambda);
  }

  bool operator> (const RGBPixelorderKLM<TInputImage,TOutputImage>* rhs) const
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


}; // class RGBPixelorderKLM


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBPixelorderKLM.txx"
#endif



#endif
