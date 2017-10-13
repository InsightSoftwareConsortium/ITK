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
#ifndef itkPathSource_h
#define itkPathSource_h

#include "itkProcessObject.h"
#include "itkPath.h"

namespace itk
{
/** \class PathSource
 *  \brief Base class for all process objects that output path data.
 *
 * PathSource is the base class for all process objects that output
 * path data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output path. The class also defines
 * some internal private data members that are used to manage streaming
 * of data.
 *
 * \ingroup DataSources
 * \ingroup Paths
 * \ingroup ITKPath
 */

template< typename TOutputPath >
class ITK_TEMPLATE_EXPORT PathSource:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef PathSource                 Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathSource, ProcessObject);

  /** Some convenient typedefs. */
  typedef TOutputPath                         OutputPathType;
  typedef typename OutputPathType::Pointer    OutputPathPointer;
  typedef typename OutputPathType::InputType  OutputPathInputType;
  typedef typename OutputPathType::OutputType OutputPathOutputType;
  typedef typename OutputPathType::IndexType  OutputPathIndexType;
  typedef typename OutputPathType::OffsetType OutputPathOffsetType;

  /** Get the output data of this process object.  The output of this
   * function is not valid until an appropriate Update() method has
   * been called, either explicitly or implicitly.  Both the filter
   * itself and the data object have Update() methods, and both
   * methods update the data.  Here are three ways to use
   * GetOutput() and make sure the data is valid.  In these
   * examples, \a image is a pointer to some Image object, and the
   * particular ProcessObjects involved are filters.  The same
   * examples apply to non-image (e.g. Mesh) data as well.
   *
   * \code
   *   anotherFilter->SetInput( someFilter->GetOutput() );
   *   anotherFilter->Update();
   * \endcode
   *
   * In this situation, \a someFilter and \a anotherFilter are said
   * to constitute a \b pipeline.
   *
   * \code
   *   image = someFilter->GetOutput();
   *   image->Update();
   * \endcode
   *
   * \code
   *   someFilter->Update();
   *   image = someFilter->GetOutput();
   * \endcode
   * (In the above example, the two lines of code can be in
   * either order.)
   *
   * Note that Update() is not called automatically except within a
   * pipeline as in the first example.  When \b streaming (using a
   * StreamingImageFilter) is activated, it may be more efficient to
   * use a pipeline than to call Update() once for each filter in
   * turn.
   *
   * For an image, the data generated is for the requested
   * Region, which can be set using ImageBase::SetRequestedRegion().
   * By default, the largest possible region is requested.
   */
  OutputPathType * GetOutput();

  OutputPathType * GetOutput(unsigned int idx);

  /** Graft the specified DataObject onto this ProcessObject's output.
   * This method grabs a handle to the specified DataObject's path
   * data to use as its output's own path data. It also copies the
   * region ivars (RequestedRegion, BufferedRegion,
   * LargestPossibleRegion) and meta-data (Spacing, Origin) from the
   * specified data object into this filter's output data object. Most
   * importantly, however, it leaves the Source ivar untouched so the
   * original pipeline routing is intact. This method is used when a
   * process object is implemented using a mini-pipeline which is
   * defined in its GenerateData() method.  The usage is:
   *
   * \code
   *    // setup the mini-pipeline to process the input to this filter
   *    firstFilterInMiniPipeline->SetInput( this->GetInput() );
   *
   *    // setup the mini-pipeline to calculate the correct regions
   *    // and write to the appropriate bulk data block
   *    lastFilterInMiniPipeline->GraftOutput( this->GetOutput() );
   *
   *    // execute the mini-pipeline
   *    lastFilterInMiniPipeline->Update();
   *
   *    // graft the mini-pipeline output back onto this filter's output.
   *    // this is needed to get the appropriate regions passed back.
   *    this->GraftOutput( lastFilterInMiniPipeline->GetOutput() );
   * \endcode
   *
   * For proper pipeline execution, a filter using a mini-pipeline
   * must implement the GeneratseInputRequestedRegion(),
   * GenerateOutputRequestedRegion(), GenerateOutputInformation() and
   * EnlargeOutputRequestedRegion() methods as necessary to reflect
   * how the mini-pipeline will execute (in other words, the outer
   * filter's pipeline mechanism must be consistent with what the
   * mini-pipeline will do). */
  // just calls GraftNthOutput()
  virtual void GraftOutput(OutputPathType *output);

  /** Graft the specified data object onto this ProcessObject's idx'th
   * output. This is the similar to GraftOutput method except is
   * allows you specify which output is affected. The specified index
   * must be a valid output number (less than
   * ProcessObject::GetNumberOfIndexedOutputs()). See the GraftOutput for
   * general usage information. */
  virtual void GraftNthOutput(unsigned int idx, OutputPathType *output);

  /** Make a DataObject of the correct type to used as the specified
   * output.  Every ProcessObject subclass must be able to create a
   * DataObject that can be used as a specified output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.  DataObject::DisconnectPipeline, disconnects a data object
   * from being an output of its current source.  When the data object
   * is disconnected, the ProcessObject needs to construct a replacement
   * output data object so that the ProcessObject is in a valid state.
   * So DataObject::DisconnectPipeline eventually calls
   * ProcessObject::MakeOutput. Note that MakeOutput always returns a
   * SmartPointer to a DataObject. If a subclass of ImageSource has
   * multiple outputs of different types, then that class must provide
   * an implementation of MakeOutput(). */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

protected:
  PathSource();
  virtual ~PathSource() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  // Inherit the empty ProcessObject::GenerateData()

  // Inherit ProcessObject::PrepareOutputs(), which calls Initialize()
  // (Image replaces w/ empty function)

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PathSource);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathSource.hxx"
#endif

#endif
