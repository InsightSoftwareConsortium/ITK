/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodOperatorImageFunction_h
#define _itkNeighborhoodOperatorImageFunction_h

#include "itkImageFunction.h"
#include "itkNeighborhood.h"

namespace itk
{

/**
 * \class NeighborhoodOperatorImageFunction
 * \brief Compute the convolution of a neighborhood operator with the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 */
template <class TInputImage,class TOutput>
class ITK_EXPORT NeighborhoodOperatorImageFunction :
  public ImageFunction< TInputImage, TOutput >
{
public:

  /**Standard "Self" typedef */
  typedef NeighborhoodOperatorImageFunction Self;

  /** Standard "Superclass" typedef*/
  typedef ImageFunction<TInputImage, TOutput> Superclass;

  /** Smart pointer typedef support. */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory.*/
  itkNewMacro(Self);

  /** InputImageType typedef support.*/
  typedef TInputImage                                     InputImageType;
  typedef typename InputImageType::PixelType              InputPixelType;
  typedef typename Superclass::IndexType                  IndexType;
  typedef typename Superclass::ContinuousIndexType        ContinuousIndexType;

  /** Run-time type information (and related methods). */
  itkTypeMacro( NeighborhoodOperatorImageFunction, ImageFunction );


  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);
  
  typedef Neighborhood<InputPixelType, itkGetStaticConstMacro(ImageDimension)> NeighborhoodType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Set the input image. */
  //virtual void SetInputImage( InputImageType * ptr );

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline). */
  void SetOperator(const NeighborhoodType &p) const
    {
    m_Operator = p;
    this->Modified();
    }
  
  /** Evalutate the  in the given dimension at specified point 
   *  Subclasses should override this method. */
   virtual TOutput Evaluate(const PointType& ) const 
   {
    std::cout << "NeighborhoodOperatorImageFunction::Evaluate(): Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
   }
  /** Evaluate the function at specified Index position*/
  virtual TOutput EvaluateAtIndex( const IndexType & index ) const;

  /** Evaluate the function at specified ContinousIndex position.
   * Subclasses should override this method. */
  virtual TOutput EvaluateAtContinuousIndex( 
    const ContinuousIndexType &  ) const 
  {
    std::cout << "NeighborhoodOperatorImageFunction::EvaluateAtContinuousIndex():Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
  }

protected:
  NeighborhoodOperatorImageFunction();
  NeighborhoodOperatorImageFunction( const Self& ){};

  ~NeighborhoodOperatorImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  mutable NeighborhoodType m_Operator;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorImageFunction.txx"
#endif

#endif

