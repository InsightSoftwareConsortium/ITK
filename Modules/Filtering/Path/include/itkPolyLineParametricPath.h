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
#ifndef itkPolyLineParametricPath_h
#define itkPolyLineParametricPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkIndex.h"

namespace itk
{
/** \class PolyLineParametricPath
 * \brief  Represent a path of line segments through ND Space
 *
 * This class is intended to represent parametric paths through an image, where
 * the paths are composed of line segments.  Each line segment traverses one
 * unit of input.  A classic application of this class is the representation of
 * contours in 2D images, especially when the contours only need to be
 * approximately correct.  Another use of a path is to guide the movement of an
 * iterator through an image.
 *
 * \sa EllipseParametricPath
 * \sa FourierSeriesPath
 * \sa OrthogonallyCorrectedParametricPath
 * \sa ParametricPath
 * \sa ChainCodePath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 *
 * \wiki
 * \wikiexample{Curves/PolyLineParametricPath,A data structure for a piece-wise linear curve}
 * \endwiki
 */
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT PolyLineParametricPath:public
  ParametricPath< VDimension >
{
public:
  /** Standard class typedefs. */
  typedef PolyLineParametricPath       Self;
  typedef ParametricPath< VDimension > Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PolyLineParametricPath, ParametricPath);

  /** Input type */
  typedef typename Superclass::InputType InputType;

  /** Output type */
  typedef typename Superclass::OutputType OutputType;

  /** Basic data-structure types used */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;
  typedef Index<  VDimension >                     IndexType;
  typedef Offset< VDimension >                     OffsetType;
  typedef Point< double, VDimension >              PointType;
  typedef Vector< double, VDimension >             VectorType;
  typedef ContinuousIndexType                      VertexType;
  typedef VectorContainer< unsigned, VertexType >  VertexListType;
  typedef typename VertexListType::Pointer         VertexListPointer;

  /** Return the location of the parametric path at the specified location. */
  virtual OutputType Evaluate(const InputType & input) const ITK_OVERRIDE;

  ///** Evaluate the first derivative of the ND output with respect to the 1D
  //  * input.  This is an exact, algebraic function. */
  //virtual VectorType EvaluateDerivative(const InputType & input) const;

  /** Add a vertex (and a connecting line segment to the previous vertex).
   * Adding a vertex has the additional effect of extending the domain of the
   * PolyLineParametricPath by 1.0 (each pair of consecutive verticies is
   * separated by one unit of input). */
  inline void AddVertex(const ContinuousIndexType & vertex)
  {
    m_VertexList->InsertElement(m_VertexList->Size(), vertex);
    this->Modified();
  }

  /** Where does the path end?  This value is necessary for IncrementInput() to
   * know how to go to the end of a path.  Since each line segment covers one
   * unit of input, this is the number of verticies - 1. */
  virtual InputType EndOfInput() const ITK_OVERRIDE
  {
    return m_VertexList->Size() - 1;
  }

  /** New() method for dynamic construction */
  itkNewMacro(Self);

  /** Needed for Pipelining */
  virtual void Initialize(void) ITK_OVERRIDE
  {
    m_VertexList->Initialize();
  }

  /** Return the container of Vertices as a const object. */
  itkGetModifiableObjectMacro(VertexList, VertexListType);

  /** This function overrides the superclass IncrementInput and calculates
   *  the next pixel along the path to visit by using the instantaneous
   *  partial derivatives to calculate the timestep needed to move along the
   *  path by one pixel */
  virtual OffsetType IncrementInput(InputType & input) const ITK_OVERRIDE;

  /** This function overrides the superclass EvaluateDerivative and instead
   *  calculates the instantaneous derivative of input by taking the index
   *  of the previous and next integral timepoints and subtracting them */
  virtual VectorType EvaluateDerivative(const InputType & input) const ITK_OVERRIDE;

protected:
  PolyLineParametricPath();
  ~PolyLineParametricPath() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolyLineParametricPath);

  VertexListPointer m_VertexList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolyLineParametricPath.hxx"
#endif

#endif
