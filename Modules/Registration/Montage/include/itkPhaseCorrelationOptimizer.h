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
#ifndef itkPhaseCorrelationOptimizer_h
#define itkPhaseCorrelationOptimizer_h

#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{

/** \class PhaseCorrelationOptimizer
 *
 *  \brief Defines common interface for optimizers, that estimates the shift
 *         from correlation surface.
 *
 *  The class is templated over the input image type, as some optimizers operate
 *  real correlation surface and some on complex correlation surface.
 *
 *  This class implements input and output handling, while the computation has
 *  to be performed by ComputeOffset() method, that must be overriden in derived
 *  classes.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT PhaseCorrelationOptimizer: public ProcessObject
{
public:
  typedef PhaseCorrelationOptimizer Self;
  typedef ProcessObject             Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhaseCorrelationOptimizer, ProcessObject);

  /**  Type of the input image. */
  typedef          TImage                             ImageType;
  typedef typename ImageType::ConstPointer            ImageConstPointer;

  /** Dimensionality of input and output data. */
  itkStaticConstMacro( ImageDimension, unsigned int, ImageType::ImageDimension );

  /** Type for the output parameters.
   *  It defines a position in the optimization search space. */
  typedef typename ImageType::PointType               OffsetType;
  typedef typename OffsetType::ValueType              OffsetScalarType;

  /** Type for the output: Using Decorator pattern for enabling
  *  the offset to be passed in the data pipeline */
  typedef  SimpleDataObjectDecorator< OffsetType >    OffsetOutputType;
  typedef typename OffsetOutputType::Pointer          OffsetOutputPointer;
  typedef typename OffsetOutputType::ConstPointer     OffsetOutputConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer                DataObjectPointer;

  /** Get the computed offset. */
  itkGetConstReferenceMacro( Offset, OffsetType );

  /** Sets the input image to the optimizer. */
  void SetInput( const ImageType * image );

  /** Returns the offset resulting from the registration process  */
  const OffsetOutputType * GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   *  output. */
  DataObjectPointer MakeOutput( DataObjectPointerArraySizeType idx ) override;

protected:
  PhaseCorrelationOptimizer();
  virtual ~PhaseCorrelationOptimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const override;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the output values. */
  void GenerateData () override;


  /** This method is executed by this type and must be reimplemented by child
   *  filter to perform the computation.
   */
  virtual void ComputeOffset() = 0;

  /** Provides derived classes with the ability to set this private var */
  itkSetMacro( Offset, OffsetType );

private:
  PhaseCorrelationOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OffsetType    m_Offset;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhaseCorrelationOptimizer.hxx"
#endif

#endif
