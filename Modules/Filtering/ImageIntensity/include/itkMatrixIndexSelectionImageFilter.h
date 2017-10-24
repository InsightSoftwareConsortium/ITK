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
#ifndef itkMatrixIndexSelectionImageFilter_h
#define itkMatrixIndexSelectionImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
template< typename TInput, typename TOutput >
class MatrixIndexSelection
{
public:
  MatrixIndexSelection() { m_I = m_J = 0; }
  ~MatrixIndexSelection() {}

  void GetIndices(unsigned int & i, unsigned int & j) const { i = m_I; j = m_J; }
  void SetIndices(unsigned int i, unsigned int j) { m_I = i; m_J = j; }

  bool operator!=(const MatrixIndexSelection & other) const
  {
    if ( m_I != other.m_I
         || m_J != other.m_J  )
      {
      return true;
      }
    return false;
  }

  bool operator==(const MatrixIndexSelection & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( A[m_I][m_J] );
  }

private:
  unsigned int m_I;
  unsigned int m_J;
};
}

/** \class MatrixIndexSelectionImageFilter
 *
 * \brief Extracts the selected indices of a matrix image that is the input
 * pixel type
 *
 * This filter is templated over the input image type and
 * output image type.
 *
 * The filter expect the input image pixel type to be a matrix and
 * the output image pixel type to be a scalar. The only requirement on
 * the type used for representing the vector is that it must provide an
 * operator(i,j).
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 */

template< typename TInputImage, typename TOutputImage >
class MatrixIndexSelectionImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::MatrixIndexSelection< typename TInputImage::PixelType,
                                                          typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef MatrixIndexSelectionImageFilter
  Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::MatrixIndexSelection< typename TInputImage::PixelType,
                                                                  typename TOutputImage::PixelType > > Superclass;
  typedef SmartPointer< Self >
  Pointer;
  typedef SmartPointer< const Self >
  ConstPointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(MatrixIndexSelectionImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Get/Set methods for the index */
  void SetIndices(unsigned int i, unsigned int j)
  {
    this->GetFunctor().SetIndices(i, j);
    this->Modified();
  }

  void GetIndices(unsigned int & i, unsigned int & j) const
  { return this->GetFunctor().GetIndices(i, j); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  MatrixIndexSelectionImageFilter() {}
  virtual ~MatrixIndexSelectionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MatrixIndexSelectionImageFilter);
};
} // end namespace itk

#endif
