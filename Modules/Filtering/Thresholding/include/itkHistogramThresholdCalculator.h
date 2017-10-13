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

#ifndef itkHistogramThresholdCalculator_h
#define itkHistogramThresholdCalculator_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkProgressReporter.h"

namespace itk
{

/** \class HistogramThresholdCalculator
 * \brief Base class to compute a threshold value based on the histogram of an image
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/10380/3279  or
 * http://www.insight-journal.org/browse/publication/811
 *
 * \ingroup Operators
 * \ingroup ITKThresholding
 */
template <typename THistogram, typename TOutput>
class HistogramThresholdCalculator : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef HistogramThresholdCalculator    Self;
  typedef ProcessObject                   Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramThresholdCalculator, ProcessObject);

  /** Type definition for the input histogram. */
  typedef THistogram HistogramType;

  /** output object type */
  typedef TOutput                               OutputType;
  typedef SimpleDataObjectDecorator<OutputType> DecoratedOutputType;

  void SetInput( const HistogramType * input )
  {
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 0, const_cast< HistogramType * >( input ) );
  }

  const HistogramType * GetInput() const
  {
    if ( this->GetNumberOfInputs() < 1 )
      {
      return ITK_NULLPTR;
      }
    return static_cast< const HistogramType * >( this->ProcessObject::GetInput(0) );
  }

  DecoratedOutputType * GetOutput()
  {
    if ( this->GetNumberOfOutputs() < 1 )
      {
      return ITK_NULLPTR;
      }
    return static_cast< DecoratedOutputType * >( this->ProcessObject::GetOutput(0) );
  }

  using Superclass::MakeOutput;
  virtual typename DataObject::Pointer MakeOutput(DataObjectPointerArraySizeType) ITK_OVERRIDE
  {
    return DecoratedOutputType::New().GetPointer();
  }

  const OutputType & GetThreshold()
  {
    if ( this->GetNumberOfOutputs() < 1 )
      {
      itkExceptionMacro(<<"No output available.");
      }
    return static_cast< DecoratedOutputType * >( this->ProcessObject::GetOutput(0) )->Get();
  }

protected:
  HistogramThresholdCalculator()
  {
    this->ProcessObject::SetNumberOfRequiredOutputs(1);
    this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );
  }
  virtual ~HistogramThresholdCalculator() ITK_OVERRIDE {};
  using ProcessObject::SetInput;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HistogramThresholdCalculator);

};

} // end namespace itk

#endif
