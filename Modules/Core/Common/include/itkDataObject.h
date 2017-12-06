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
#ifndef itkDataObject_h
#define itkDataObject_h

#include "itkObject.h"
#include "itkSmartPointerForwardReference.h"
#include "itkMacro.h"
#include "itkRealTimeStamp.h"

namespace itk
{
// Forward reference because of circular dependencies
class ITK_FORWARD_EXPORT ProcessObject;
class ITK_FORWARD_EXPORT DataObject;

/*--------------------Data Object Exceptions---------------------------*/

/** \class DataObjectError
 * \brief Exception object for DataObject exceptions.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT DataObjectError:public ExceptionObject
{
public:
  /** Default constructor.  Needed to ensure the exception object can be
   * copied. */
  DataObjectError();

  /** Destructor. Need to specify empty throw() to avoid warnings. */
  virtual ~DataObjectError() ITK_NOEXCEPT ITK_OVERRIDE {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  DataObjectError(const char *file, unsigned int lineNumber);

  /** Constructor. Needed to ensure the exception object can be copied. */
  DataObjectError(const std::string & file, unsigned int lineNumber);

  /** Copy constructor. Needed to ensure the exception object can be copied. */
  DataObjectError(const DataObjectError & orig);

  /** Operator=.  Needed to ensure the exception object can be copied. */
  DataObjectError & operator=(const DataObjectError & orig);

  /** Standard type macro */
  itkTypeMacro(DataObjectError, ExceptionObject);

  /** Set the data object that is throwing this exception. */
  void SetDataObject(DataObject *dobj);

  /** Get the data object that is throwing this exception. */
  DataObject * GetDataObject();

protected:
  /** Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the "thrower".   */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:
  DataObject *m_DataObject;
};

/** \class InvalidRequestRegionError
 *  \brief Exception object for invalid requested region.
 *
 * Exception object for invalid requested region.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT InvalidRequestedRegionError:public DataObjectError
{
public:
  /** Default constructor. Needed to ensure the exception object can be copied.
    */
  InvalidRequestedRegionError();

  /** Destructor. Need to specify empty throw() to avoid warnings. */
  virtual ~InvalidRequestedRegionError() ITK_NOEXCEPT ITK_OVERRIDE {}

  /** Constructor. Needed to ensure the exception object can be copied. */
  InvalidRequestedRegionError(const char *file, unsigned int lineNumber);

  /** Constructor. Needed to ensure the exception object can be copied. */
  InvalidRequestedRegionError(const std::string & file, unsigned int lineNumber);

  /** Copy constructor.  Needed to ensure the exception object can be copied. */
  InvalidRequestedRegionError(const InvalidRequestedRegionError & orig);

  /** Operator=.  Needed to ensure the exception object can be copied. */
  InvalidRequestedRegionError & operator=(const InvalidRequestedRegionError & orig);

  /** Standard type macro */
  itkTypeMacro(InvalidRequestedRegionError, DataObjectError);

protected:
  /** Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the "thrower".   */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};

/*----------------------------Data Object--------------------------------*/

/** \class DataObject
 * \brief Base class for all data objects in ITK.
 *
 * This is the base class for all data objects in the Insight data
 * processing pipeline. A data object is an object that represents and
 * provides access to data. ProcessObjects (i.e., filters) operate on
 * input data objects, producing new data objects as output.
 * ProcessObject and DataObject are connected together into data flow
 * pipelines.
 *
 * The data flow pipeline architecture requires that DataObjects and
 * ProcessObjects negotiate the flow of information.  When the tail of
 * a pipeline is instructed to Update(), a series of requests are
 * propagated up the pipeline (from a ProcessObject to its inputs
 * (DataObjects), from these inputs to their sources (ProcessObjects),
 * etc.). A call to Update() entails 3 passes up the pipeline (though
 * not all passes will traverse the entire pipeline).  The first pass
 * up the pipeline determines when various components of the pipeline
 * were last modified and hence which components will need to be
 * updated.  As this first pass in unwinding, meta information about
 * the DataObjects (for instance image spacing and data size) are
 * passed down the pipeline.  The second pass up the pipeline
 * propagates a request for a specific piece of information (for
 * instance a sub-region of an image). A request for a piece of a
 * DataObject is propagated to its source, from there to its inputs,
 * etc. allowing each ProcessObject to determine whether (1) it can
 * already satisfy the request (the requested block of data is already
 * available) or (2) the ProcessObject will need to request a new
 * block of data on input to satisfy the output request.  Finally, a
 * pass is made up the pipeline to actually calculate the values for
 * the various blocks of data requested (i.e. pixel values are finally
 * calculated).  This final pass will only traverse up the pipeline as
 * far as the first two passes have identified.  For instance, to
 * satisfy a given request at the tail of a pipeline, only the lower
 * few ProcessObjects may have to re-execute.
 *
 * There are three types of information negotiated by the pipeline
 * (prior to actual calculation of the bulk data): modified times,
 * meta data, and regions.  The modified times keep track of when
 * various data objects were last modified and/updated and the when
 * the various process objects were modified.  The meta data is any
 * extra information about the data object that is not part of the
 * bulk data.  For instance, an Image maintains pixel spacing and
 * origin meta data.  Finally, the pipeline negotiation process passes
 * requests up the pipeline in the form of Regions.  A DataObject can
 * have as many as three regions (which themselves could be considered
 * meta data): LargestPossibleRegion, RequestedRegion, and
 * BufferedRegion.  The LargestPossibleRegion is the entirety of the
 * dataset (for instance how big is the dataset on disk).
 * LargestPossibleRegions are negotiated during the first pass of a
 * pipeline update (via the method
 * ProcessObject::GenerateOutputInformation() which is called from
 * ProcessObject::UpdateOutputInformation().  The RequestedRegion is
 * the amount of the DataObject that is requested by the user or
 * pipeline. RequestedRegions are negotiated during the second pass of
 * a pipeline update (via the methods
 * ProcessObject::EnlargeOutputRequestedRegion(),
 * ProcessObject::GenerateOutputRequestedRegion(),
 * ProcessObject::GenerateInputRequestedRegion() which are called from
 * ProcessObject::PropagateRequestedRegion()). The BufferedRegion is
 * the amount of the DataObject that is currently in memory.
 * BufferedRegions are defined during the final pass of a pipeline
 * update (when ProcessObjects finally calculate the bulk data via the
 * methods ProcessObject::GenerateData() or
 * ProcessObject::ThreadedGenerateData() which are called by
 * ProcessObject::UpdateOutputData()). These three regions can be
 * different but must satisfy the relationship RequestedRegion <=
 * BufferedRegion <= LargestPossibleRegion. For instance, an Image
 * could be 512x512x200 on disk (LargestPossibleRegion) but the
 * application may only have a 256x256x50 section of the dataset in
 * memory (BufferedRegion) and the user wants to operate on a
 * 100x100x1 section of the buffer (RequestedRegion).
 *
 * Region negotiation is not applicable for all types of DataObjects.
 * For instance, an EquivalencyTable of segmentation labels can be
 * passed from ProcessObject to ProcessObject as any other DataObject
 * but an EquivalencyTable does not support the concept of a
 * sub-region. Therefore, the region negotiations at the DataObject
 * (superclass) level are implemented as "abstract" concepts (not to
 * be confused with a C++ abstract methods), allowing subclasses to
 * provide specialized implementations on an as needed basis. There
 * are five methods provided in DataObject that a subclass of
 * DataObject may have to override for that particular type of
 * DataObject to flow through the pipeline. These methods should only
 * have to be specialized for DataObjects that do support
 * regions. These methods are:
 *
 * void UpdateOutputInformation(): This method implements the first
 * pass of the pipeline update mechanism outlined above.  It is
 * responsible for identifying when upstream components of the
 * pipeline have been change (ModifiedTimes and Pipeline
 * ModifiedTimes) and is responsible for propagating meta data through
 * the pipeline. In the simplest case, this method simply calls the
 * DataObject's source's UpdateOutputInformation() method (this is the
 * default implementation). For DataObjects that support streaming,
 * this method also propagates LargestPossibleRegions to downstream
 * ProcessObjects.
 *
 * bool VerifyRequestedRegion(): Verify that the RequestedRegion is
 * within the LargestPossibleRegion.  For DataObjects that do not
 * support Regions, this method always returns true.
 *
 * bool RequestedRegionIsOutsideOfTheBufferedRegion(): Determine
 * whether the RequestedRegion is outside of the current
 * BufferedRegion. This method is used by the second pass of a
 * pipeline update outlined above.  It is used to determine whether a
 * filter needs to re-execute in order to satisfy a given request. For
 * DataObjects that do not support Regions, this method always returns
 * false. By always returning false, these types of DataObjects will
 * update solely on the basis of modified times (whereas Images
 * update based on either modified times or the RequestedRegion
 * needs). If this method always returned true, the DataObject would
 * be updated on every single call to Update() (not recommended).
 *
 * void SetRequestedRegion(const DataObject *): Sets the RequestedRegion of
 * this DataObject to match the RequestedRegion of the DataObject that
 * is passed in as a parameter. This method is used by
 * ProcessObject::GenerateOutputRequestedRegion() and by
 * ProcessObject::SetNthOutput().  In the former case, it used as an
 * abstract API so that a ProcessObject can copy a requested region
 * from one output to all its outputs without knowing the particular
 * subclass of DataObject.  In the latter case, it used when a
 * ProcessObject has to create an output object to replace one of its
 * outputs (and needs to copy the former object's RequestedRegion). In
 * either case, it allows ProcessObject to perform these actions
 * without knowing the specifics about the particular subclass of
 * DataObject. For DataObjects that do not support Regions, this
 * method does nothing.
 *
 * void SetRequestedRegionToLargestPossibleRegion(): Sets the
 * RequestedRegion of this DataObject to match its
 * LargestPossibleRegion.  This method is used to force a filter to
 * produce all of its output on the next call to Update(). For
 * DataObjects that do not support Regions, this method does nothing.
 *
 *
 * \sa ProcessObject
 * \sa ImageBase
 * \sa Mesh
 * \ingroup DataRepresentation
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITK_FORCE_EXPORT_MACRO(ITKCommon) DataObject:public Object
{
public:
  /** Standard class typedefs. */
  typedef DataObject                 Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef std::string                DataObjectIdentifierType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DataObject, Object);

  /** Separate this data object from the pipeline.  This routine disconnects
   * a data object from the upstream pipeline. Hence an Update() from
   * downstream will not propagate back past this data object.  To completely
   * isolate this data object from the pipeline, the application must remove
   * this data object from any filters which it is connected as the input. */
  void DisconnectPipeline();

  /** Get the process object that generated this data object.
   * If there is no process object, then the data object has
   * been disconnected from the pipeline, or the data object
   * was created manually. (Note: we cannot use the GetObjectMacro()
   * defined in itkMacro because the mutual dependency of
   * DataObject and ProcessObject causes compile problems. Also,
   * a forward reference smart pointer is returned, not a smart pointer,
   * because of the circular dependency between the process and data object.)
   *
   * GetSource() returns a SmartPointerForwardReference and not a WeakPointer
   * because it is assumed the code calling GetSource() wants to hold a
   * long term reference to the source. */
  SmartPointerForwardReference< ProcessObject > GetSource() const;

  /** Which of the source's outputs corresponds to this data object? */
  const DataObjectIdentifierType & GetSourceOutputName() const;

  /** Which of the source's outputs corresponds to this data object? */
  typedef std::vector< Pointer >::size_type DataObjectPointerArraySizeType;
  DataObjectPointerArraySizeType GetSourceOutputIndex() const;

  /** Restore the data object to its initial state. This means releasing
   * memory. */
  virtual void Initialize();

  /** Turn on/off a flag to control whether this object's data is released
   * after being used by a filter.  */
  void SetReleaseDataFlag(bool flag)
  {
    m_ReleaseDataFlag = flag;
  }

  itkGetConstReferenceMacro(ReleaseDataFlag, bool);
  itkBooleanMacro(ReleaseDataFlag);

  /** Turn on/off a flag to control whether every object releases its data
   * after being used by a filter. Being a global flag, it controls the
   * behavior of all DataObjects and ProcessObjects. */
  static void SetGlobalReleaseDataFlag(bool val);

  static bool GetGlobalReleaseDataFlag();

  static void GlobalReleaseDataFlagOn()
  { Self::SetGlobalReleaseDataFlag(true); }
  static void GlobalReleaseDataFlagOff()
  { Self::SetGlobalReleaseDataFlag(false); }

  /** Release data back to system to conserve memory resource. Used during
   * pipeline execution.  Releasing this data does not make
   * down-stream data invalid, so it does not modify the MTime of this data
   * object.   */
  void ReleaseData();

  /** Return flag indicating whether data should be released after use
   * by a filter.  */
  bool ShouldIReleaseData() const;

  /** Get the flag indicating the data has been released.  */
  bool GetDataReleased() const
  { return m_DataReleased; }

  /** Provides opportunity for the data object to insure internal
   * consistency before access. Also causes owning source/filter (if
   * any) to update itself. The Update() method is composed of
   * UpdateOutputInformation(), PropagateRequestedRegion(), and
   * UpdateOutputData(). This method may call methods that throw an
   * InvalidRequestedRegionError exception. This exception will leave
   * the pipeline in an inconsistent state.  You will need to call
   * ResetPipeline() on the last ProcessObject in your pipeline in
   * order to restore the pipeline to a state where you can call
   * Update() again. */
  virtual void Update();

  /** Update the information for this DataObject so that it can be used
   * as an output of a ProcessObject.  This method is used in the pipeline
   * mechanism to propagate information and initialize the meta data
   * associated with a DataObject.  Any implementation of this method in
   * a derived class is assumed to call its source's
   * ProcessObject::UpdateOutputInformation() which determines modified
   * times, LargestPossibleRegions, and any extra meta data like spacing,
   * origin, etc. Default implementation simply call's it's source's
   * UpdateOutputInformation(). */
  virtual void UpdateOutputInformation();

  /** Methods to update the pipeline. Called internally by the
   * pipeline mechanism. */
  virtual void PropagateRequestedRegion();

  virtual void UpdateOutputData();

  /** Reset the pipeline. If an exception is thrown during an Update(),
   * the pipeline may be in an inconsistent state.  This method clears
   * the internal state of the pipeline so Update() can be called. */
  virtual void ResetPipeline();

  /** The maximum MTime of all upstream filters and data objects.
   * This does not include the MTime of this data object. */
  void SetPipelineMTime(ModifiedTimeType time)
  { m_PipelineMTime = time; }
  itkGetConstReferenceMacro(PipelineMTime, ModifiedTimeType);

  /** MTime for the last time this DataObject was generated. */
  virtual ModifiedTimeType GetUpdateMTime() const;

  /** RealTime stamp for the last time this DataObject was generated.
   *  By default, the real time stamp is initialized to the origin of
   *  the Unix epoch. That is the time 00:00:00 UTC on 1 January 1970
   *  (or 1970-01-01T00:00:00Z ISO 8601)
   */
  itkSetMacro( RealTimeStamp, RealTimeStamp );
  itkGetConstReferenceMacro( RealTimeStamp, RealTimeStamp );

  /** Setup a DataObject to receive new data.  This method is called
   * by the pipeline mechanism on each output of filter that needs
   * to execute.  The default implementation is to return a DataObject
   * to its initial state.  This may involve releasing previously
   * allocated bulk data.  Subclasses of DataObject may want to
   * override this method and/or the Initialize() method if they
   * want a different default behavior (for instance a DataObject
   * might want finer control over its bulk data memory management). */
  virtual void PrepareForNewData()
  { this->Initialize(); }

  /** Inform the pipeline mechanism that data has been generated.  This
   * method is called by ProcessObject::UpdateOutputData() once the
   * process object has finished generating its data. This essentially
   * marks the DataObject as being updated and ready for use. */
  virtual void DataHasBeenGenerated();

  /** Set the RequestedRegion to the LargestPossibleRegion.  This
   * forces a filter to produce all of the output in one execution
   * (i.e. not streaming) on the next call to Update(). */
  virtual void SetRequestedRegionToLargestPossibleRegion() {}

  /** Determine whether the RequestedRegion is outside of the
   * BufferedRegion. This method returns true if the RequestedRegion
   * is outside the BufferedRegion (true if at least one pixel is
   * outside). This is used by the pipeline mechanism to determine
   * whether a filter needs to re-execute in order to satisfy the
   * current request.  If the current RequestedRegion is already
   * inside the BufferedRegion from the previous execution (and the
   * current filter is up to date), then a given filter does not need
   * to re-execute */
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion()
  { return false; }

  /** Verify that the RequestedRegion is within the LargestPossibleRegion.
   *
   * If the RequestedRegion is not within the LargestPossibleRegion,
   * then the filter cannot possibly satisfy the request. This method
   * returns true if the request can be satisfied (even if it will be
   * necessary to process the entire LargestPossibleRegion) and
   * returns false otherwise.  This method is used by
   * PropagateRequestedRegion().  PropagateRequestedRegion() throws a
   * InvalidRequestedRegionError exception if the requested region is
   * not within the LargestPossibleRegion. Default implementation
   * simply returns true in order to support DataObjects that do not
   * need regions (for instance itk::EquivalencyTable). */
  virtual bool VerifyRequestedRegion() { return true; }

  /** Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * The default implementation of this method is empty. If a subclass
   * overrides this method, it should always call its superclass'
   * version. */
  virtual void CopyInformation(const DataObject *) {}

  /** Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  For
   * DataObject's that do not support Regions, this method does
   * nothing. Subclasses of DataObject that do support Regions,
   * provide an alternative implementation. */
  virtual void SetRequestedRegion(const DataObject *) {}

  /** Method for grafting the content of one data object into another one.
   * This method is intended to be overloaded by derived classes. Each one of
   * them should use dynamic_casting in order to verify that the grafted
   * object is actually of the same type as the class on which the Graft()
   * method was invoked. */
  virtual void Graft(const DataObject *) {}

protected:
  DataObject();
  virtual ~DataObject() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Propagate a call to ResetPipeline(). Called only from ProcessObject. */
  virtual void PropagateResetPipeline();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DataObject);

  /** Who generated this data? */
  WeakPointer< ProcessObject > m_Source;
  DataObjectIdentifierType                  m_SourceOutputName;

  /** When was this data last generated?
   *  This time stamp is an integer number and it is intended to synchronize the
   *  activities of the pipeline. It doesn't relates to the clock time of
   *  acquiring or processing the data. */
  TimeStamp m_UpdateMTime;

  /** When, in real time, this data was generated. */
  RealTimeStamp m_RealTimeStamp;

  bool m_ReleaseDataFlag; //Data will release after use by a filter if on
  bool m_DataReleased;    //Keep track of data release during pipeline execution

  /** The maximum MTime of all upstream filters and data objects.
   * This does not include the MTime of this data object. */
  ModifiedTimeType m_PipelineMTime;

  /** Static member that controls global data release after use by filter. */
  static bool m_GlobalReleaseDataFlag;

  /** Connect the specified process object to the data object. This
   * should only be called from a process object. The second parameter
   * indicates which of the source's outputs corresponds to this data
   * object. */
  bool ConnectSource(ProcessObject *s, const DataObjectIdentifierType & name);

  /** Disconnect the specified process object from the data
   * object. This should only be called from a process object. An
   * application should call DataObject::DisconnectPipeline() if it
   * wants to disconnect a data object from a pipeline. The second
   * parameter indicates which of the source's outputs corresponds to
   * this data object. If the specified source output name does not
   * match the name cached when the data object was connected to the
   * pipeline (see ConnectSource), then nothing is done. */
  bool DisconnectSource(ProcessObject *s, const DataObjectIdentifierType & name);

  /** Friends of DataObject */
  friend class ProcessObject;
  friend class DataObjectError;
};
} // end namespace itk

#endif
