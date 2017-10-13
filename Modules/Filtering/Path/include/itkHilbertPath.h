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
#ifndef itkHilbertPath_h
#define itkHilbertPath_h

#include "itkPath.h"

#include "itkNumericTraits.h"

namespace itk
{
/** \class HilbertPath
 * \brief  Represent an n-dimensional Hilbert path for a given order
 *
 * This class is used to construct a Hilbert spacing-filling curve
 * (or path) for a given order and given dimension.  The locality-
 * preserving properties make the Hilbert path an attractive option
 * for mapping multi-dimensional data to a single array.
 *
 * The path is defined by its dimensionality and order( >= 1 ) with its
 * starting point at [0]^Dimension. The size of the path in each dimension
 * is 2^order where each discrete location is visited by that path.
 * For example, a 2-D Hilbert path of order 8 can map each pixel of a 256x256
 * image onto a single array.  More properties and visualizations can be
 * found in various places on the web.
 *
 * The implementation is based on
 * Chris Hamilton, "Compact Hilbert Indices", Technical Report CS-2006-07,
 * July 24, 2006.
 * and a direct porting of the Aldo Cortesi's python code found at
 * https://github.com/cortesi/scurve
 *
 * \author Nick Tustison
 *
 * \ingroup ITKPath
 */
template<typename TIndexValue = unsigned int, unsigned int VDimension = 3>
class ITK_TEMPLATE_EXPORT HilbertPath
: public Path<TIndexValue, Index<VDimension>, VDimension>
{
public:

  /** Standard class typedefs. */
  typedef HilbertPath<TIndexValue, VDimension>               Self;
  typedef Path<unsigned int, Index<VDimension>, VDimension>  Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( HilbertPath, Path );

  /** New() method for dynamic construction */
  itkNewMacro( Self );

  /** Dimension underlying input image. */
  itkStaticConstMacro( Dimension, unsigned int, VDimension );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;
  typedef typename Superclass::InputType  InputType;

  /** The input/output typedefs*/
  typedef InputType                             PathIndexType;
  typedef OutputType                            IndexType;
  typedef unsigned int                          HilbertOrderType;
  typedef std::vector<IndexType>                HilbertPathType;
  typedef typename HilbertPathType::size_type   HilbertPathSizeType;
  typedef typename Superclass::OffsetType       OffsetType;

  // Functions inherited from Path

  /** Evaluate the hilbert path for the index at the specified path-position. */
  virtual OutputType Evaluate( const PathIndexType & input ) const ITK_OVERRIDE
    {
    return this->m_HilbertPath[input];
    }

  virtual OutputType EvaluateToIndex( const PathIndexType & input ) const ITK_OVERRIDE
    {
    return this->m_HilbertPath[input];
    }

  /** Evaluate the hilbert path for the path-position at the specified index. */
  virtual InputType EvaluateInverse( const IndexType & input )
    {
    return this->TransformMultiDimensionalIndexToPathIndex( input );
    }

  /** Where does the path end (what is the last valid input value)? */
  virtual InputType EndOfInput() const ITK_OVERRIDE
    {
    return static_cast<InputType>( this->NumberOfSteps() );  // 0 is before the first step, 1 is after it
    }

  /** Increment the input variable passed by reference and then return the
   * index stored at the new path-position.
   */
  virtual OffsetType IncrementInput( InputType & itkNotUsed( input ) ) const ITK_OVERRIDE
    {
    itkExceptionMacro( "Not implemented." );
    }

  /** Remove all steps from the path*/
  virtual inline void Clear()
    {
    this->m_HilbertPath.clear();
    this->Modified();
    }

  /** How many steps in the path? */
  virtual inline HilbertPathSizeType NumberOfSteps() const
    {
    return m_HilbertPath.size();
    }

  /** Needed for Pipelining */
  virtual void Initialize( void ) ITK_OVERRIDE
    {
    this->Clear();
    this->ConstructHilbertPath();
    }

  /**
   * Set/get Hilbert order.  The multi-dimensional space is of size 2^(HilbertOrder).
   */
  itkSetClampMacro( HilbertOrder, HilbertOrderType, 1, NumericTraits<HilbertOrderType>::max() );
  itkGetConstMacro( HilbertOrder, HilbertOrderType );

  /** Convert the path index to the multidimensional index location */
  IndexType TransformPathIndexToMultiDimensionalIndex( const PathIndexType id );

  /** Convert the multidimensional index to the path index */
  PathIndexType TransformMultiDimensionalIndexToPathIndex( const IndexType & index );

protected:
  HilbertPath();
  ~HilbertPath() ITK_OVERRIDE {}
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HilbertPath);

  void ConstructHilbertPath();

  PathIndexType GetTransform( const PathIndexType, const PathIndexType, const PathIndexType, const PathIndexType );

  PathIndexType GetInverseTransform( const PathIndexType, const PathIndexType, const PathIndexType, const PathIndexType );

  PathIndexType GetGrayCode( const PathIndexType );

  PathIndexType GetInverseGrayCode( const PathIndexType );

  PathIndexType SetBit( const PathIndexType, const PathIndexType, const PathIndexType, const PathIndexType );

  PathIndexType GetRightBitRotation( PathIndexType, PathIndexType, const PathIndexType );

  PathIndexType GetLeftBitRotation( PathIndexType, PathIndexType, const PathIndexType );

  PathIndexType GetTrailingSetBits( const PathIndexType, const PathIndexType );

  PathIndexType GetDirection( const PathIndexType, const PathIndexType );

  PathIndexType GetEntry( const PathIndexType );

  PathIndexType GetBitRange( const PathIndexType, const PathIndexType, const PathIndexType, const PathIndexType );

  HilbertOrderType             m_HilbertOrder;
  HilbertPathType              m_HilbertPath;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHilbertPath.hxx"
#endif

#endif
