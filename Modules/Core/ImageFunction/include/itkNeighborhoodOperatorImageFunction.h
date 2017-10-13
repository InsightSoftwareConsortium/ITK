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
#ifndef itkNeighborhoodOperatorImageFunction_h
#define itkNeighborhoodOperatorImageFunction_h

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
 * \ingroup ITKImageFunction
 *
 * \wiki
 * \wikiexample{Functions/NeighborhoodOperatorImageFunction,Multiply a kernel with an image at a particular location}
 * \endwiki
 */
template< typename TInputImage, typename TOutput >
class ITK_TEMPLATE_EXPORT NeighborhoodOperatorImageFunction:
  public ImageFunction< TInputImage, TOutput >
{
public:

  /**Standard "Self" typedef */
  typedef NeighborhoodOperatorImageFunction Self;

  /** Standard "Superclass" typedef */
  typedef ImageFunction< TInputImage, TOutput > Superclass;

  /** Smart pointer typedef support. */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage                              InputImageType;
  typedef typename InputImageType::PixelType       InputPixelType;
  typedef typename Superclass::IndexType           IndexType;
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NeighborhoodOperatorImageFunction, ImageFunction);

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  typedef Neighborhood< TOutput, itkGetStaticConstMacro(ImageDimension) > NeighborhoodType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Set the input image. */
  //virtual void SetInputImage( InputImageType * ptr );

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline). */
  void SetOperator(const NeighborhoodType & p) const
  {
    m_Operator = p;
    this->Modified();
  }

  /** Evalutate the  in the given dimension at specified point
   *  Subclasses should override this method. */
  virtual TOutput Evaluate(const PointType &) const ITK_OVERRIDE
  {
    std::cout << "NeighborhoodOperatorImageFunction::Evaluate(): Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
  }

  /** Evaluate the function at specified Index position */
  virtual TOutput EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

  /** Evaluate the function at specified ContinuousIndex position.
   * Subclasses should override this method. */
  virtual TOutput EvaluateAtContinuousIndex(
    const ContinuousIndexType &) const ITK_OVERRIDE
  {
    std::cout << "NeighborhoodOperatorImageFunction::EvaluateAtContinuousIndex():Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
  }

protected:
  NeighborhoodOperatorImageFunction();
  NeighborhoodOperatorImageFunction(const Self &){}

  ~NeighborhoodOperatorImageFunction() ITK_OVERRIDE {}

  void operator=(const Self &){}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  mutable NeighborhoodType m_Operator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorImageFunction.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorImageFunction.hxx"
#endif
*/

#endif
