/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMeshSource_h
#define itkMeshSource_h


#include "itkProcessObject.h"

namespace itk
{
/** \class MeshSource
 *  \brief Base class for all process objects that output mesh data.
 *
 * MeshSource is the base class for all process objects that output
 * mesh data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output mesh. The class also defines
 * some internal private data members that are used to manage streaming
 * of data.
 *
 * \ingroup DataSources
 * \ingroup ITKMesh
 */
template <typename TOutputMesh>
class ITK_TEMPLATE_EXPORT MeshSource : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeshSource);

  /** Standard class type aliases. */
  using Self = MeshSource;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshSource, ProcessObject);

  /** Some convenient type alias. */
  using DataObjectPointer = DataObject::Pointer;
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using DataObjectIdentifierType = Superclass::DataObjectIdentifierType;

  /** Get the mesh output of this process object.  */
  OutputMeshType *
  GetOutput();

  OutputMeshType *
  GetOutput(unsigned int idx);

  /** Set the mesh output of this process object. This call is slated
   * to be removed from ITK. You should GraftOutput() and possible
   * DataObject::DisconnectPipeline() to properly change the output. */
  using Superclass::SetOutput;
  void
  SetOutput(TOutputMesh * output);

  /** Graft the specified DataObject onto this ProcessObject's output.
   * This method grabs a handle to the specified DataObject's bulk
   * data to used as its output's own bulk data. It also copies the
   * region ivars (RequestedRegion, BufferedRegion,
   * LargestPossibleRegion) and meta-data (Spacing, Origin) from the
   * specified data object into this filter's output data object. Most
   * importantly, however, it leaves the Source ivar untouched so the
   * original pipeline routing is intact. This method is used when a
   * process object is implemented using a mini-pipeline which is
   * defined in its GenerateData() method.  The usage is:
   *
     \code
        // setup the mini-pipeline to process the input to this filter
        firstFilterInMiniPipeline->SetInput( this->GetInput() );

        // setup the mini-pipeline to calculate the correct regions
        // and write to the appropriate bulk data block
        lastFilterInMiniPipeline->GraftOutput( this->GetOutput() );

        // execute the mini-pipeline
        lastFilterInMiniPipeline->Update();

        // graft the mini-pipeline output back onto this filter's output.
        // this is needed to get the appropriate regions passed back.
        this->GraftOutput( lastFilterInMiniPipeline->GetOutput() );
     \endcode
   *
   * For proper pipeline execution, a filter using a mini-pipeline
   * must implement the GenerateInputRequestedRegion(),
   * GenerateOutputRequestedRegion(), GenerateOutputInformation() and
   * EnlargeOutputRequestedRegion() methods as necessary to reflect
   * how the mini-pipeline will execute (in other words, the outer
   * filter's pipeline mechanism must be consistent with what the
   * mini-pipeline will do). */
  virtual void
  GraftOutput(DataObject * graft);

  /** Graft the specified data object onto this ProcessObject's named
   * output. This is similar to the GraftOutput method except it
   * allows you to specify which output is affected.
   * See the GraftOutput for general usage information.
   */
  virtual void
  GraftOutput(const DataObjectIdentifierType & key, DataObject * graft);

  virtual void
  GraftNthOutput(unsigned int idx, DataObject * graft);

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
   * SmartPointer to a DataObject. If a subclass of MeshSource has
   * multiple outputs of different types, then that class must provide
   * an implementation of MakeOutput(). */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

protected:
  MeshSource();
  ~MeshSource() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Requested region of Mesh is specified as i of N unstructured regions.
   * Since all DataObjects should be able to set the requested region in
   * unstructured form, just copy output->RequestedRegion all inputs. */
  void
  GenerateInputRequestedRegion() override;

private:
  /** Used by streaming: The requested region of the output being processed
   * by the execute method. Set in the GenerateInputRequestedRegion method. */
  int m_GenerateDataRegion;
  int m_GenerateDataNumberOfRegions;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeshSource.hxx"
#endif

#endif
