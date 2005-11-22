/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeLabelImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkChangeLabelImageFilter_h
#define __itkChangeLabelImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
  
/** \class ChangeLabelImageFilter
 *
 * \brief Change Sets of Labels
 *
 * This filter produces an output image whose pixels
 * are either copied from the input if they are not being changed
 * or are rewritten based on the change parameters
 * 
 * This filter is templated over the input image type
 * and the output image type.
 * 
 * The filter expect both images to have the same number of dimensions.
 *
 * \author Tim Kelliher. GE Research, Niskayuna, NY.
 * \note This work was supported by a grant from DARPA, executed by the
 *  U.S. Army Medical Research and Materiel Command/TATRC Assistance
 *  Agreement, Contract# W81XWH-05-2-0059.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */

#include <map>

namespace Functor {  
  
template< class TInput, class TOutput>
class ChangeLabel
{
public:
  ChangeLabel() {};
  ~ChangeLabel() {};

  typedef std::map<TInput, TOutput> ChangeMapType;

  TInput GetChange( const TInput & original )
  { 
    return m_ChangeMap[original]; 
  }

  void SetChange( const TInput & original, const TOutput & result )
  { 
    m_ChangeMap[original] = result; 
  }
  
  void SetChangeMap( ChangeMapType & changeMap )
  { 
    m_ChangeMap = changeMap; 
  }

  void ClearChangeMap( )
  { 
    m_ChangeMap.clear(); 
  }

  inline TOutput operator()( const TInput & A )
  {
    if ( m_ChangeMap.find(A) != m_ChangeMap.end() )
      {
      return m_ChangeMap[A];
      }
    return A;
  }

private:

  ChangeMapType m_ChangeMap;

};
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ChangeLabelImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::ChangeLabel< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType> >
{
public:
  /** Standard class typedefs. */
  typedef ChangeLabelImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Functor::ChangeLabel< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChangeLabelImageFilter, UnaryFunctorImageFilter);

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** The input pixel type must support comparison operators. */
  itkConceptMacro(PixelTypeComparable, (Concept::Comparable<InputPixelType>));

  /** Type of the change map to use for change requests */
  typedef std::map<InputPixelType, OutputPixelType> ChangeMapType;

  /** Set up a change of a single label */
  void SetChange( const InputPixelType & original, const OutputPixelType & result );
  
  /** Set the entire change map */
  void SetChangeMap( const ChangeMapType & changeMap );
  
  /** Clears the entire change map */
  void ClearChangeMap( );
  
protected:
  ChangeLabelImageFilter();
  virtual ~ChangeLabelImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  ChangeLabelImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChangeLabelImageFilter.txx"
#endif

#endif
