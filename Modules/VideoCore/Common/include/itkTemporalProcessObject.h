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
#ifndef __itkTemporalProcessObject_h
#define __itkTemporalProcessObject_h

#include "itkProcessObject.h"
#include "itkTemporalRegion.h"

namespace itk
{

/** Forward declarations */
class Region;
class TemporalDataObject;


/** \class TemporalProcessObject
 * \brief TemporalProcessObject implements a ProcessObject for the itk pipeline
 *        with the notion of a temporal region
 *
 * TemporalProcessObject acts as a pass-through in the inheritance tree in
 * order to require that subclasses properly implement handeling of temporal
 * regions. The three methods that a subclass must implement are:
 *
 * EnlargeOutputRequestedTemporalRegion(TemporalDataObject*)
 * GenerateOutputRequestedTemporalRegion(TemporalDataObject*)
 * GenerateInputRequestedTemporalRegion(TemporalDataObject*)
 *
 * These three methods mirror the similarly named methods in ProcessObject but
 * require that the inputs be TemporalDataObjects. These methods are called by
 * the corresponding RequestedRegion methods after ensuring that the DataObject
 * outputs can be cast to TemporalDataObject.
 */
class TemporalProcessObject:public ProcessObject
{
public:

  /*-TYPEDEFS----------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef TemporalProcessObject       Self;
  typedef ProcessObject               Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TemporalProcessObject, ProcessObject);


  /*-PUBLIC METHODS----------------------------------------------------------*/

  /** Override EnlargeOutputRequestedRegion, GenerateOutputRequestedRegion, and
   * GenerateInputRequestedRegion to handle temporal regions */
  virtual void EnlargeOutputRequestedRegion(DataObject* output);
  virtual void GenerateOutputRequestedRegion(DataObject* output);
  virtual void GenerateInputRequestedRegion();


  /** Get/Set the number of frames of input required to produce output */
  itkGetMacro(UnitInputNumberOfFrames, unsigned long);
  itkSetMacro(UnitInputNumberOfFrames, unsigned long);

  /** Get/Set the number of frames of output producdd for a single set
   * of input frames */
  itkGetMacro(UnitOutputNumberOfFrames, unsigned long);
  itkSetMacro(UnitOutputNumberOfFrames, unsigned long);

  /** The default implementation of UpdateOutputInformation to handle temporal
   * regions will compute the proper size of the output largest possible
   * temporal region based on the largest possible temporal region of the input,
   * the unit input/output sizes for the process, and the number of frames
   * skipped per output*/
  virtual void UpdateOutputInformation();

  /** Override ProcessObject's implementation of UpdateOutputData. This is
   * necessary because by default ProcessObject propagates the call to its
   * inputs before calling GenerateData. This doesn't work here because we need
   * to adjust the requested temporal region of our inputs before calling their
   * UpdateOutputData methods
   *
   * NOTE: Subclasses that don't use the temporal streaming process must
   * override this function to call the version implemented in ProcessObject
   *
   * Code: this->ProcessObject::UpdateOutputData( output )
   */
  virtual void UpdateOutputData(DataObject* output);

  /** Override GenerateData to do temporal region streaming. This is analogous
   * to the ThreadedGenerateData system implemented in ImageSource, but it
   * functions slightly differently. Since most temporal processes are going to
   * to need more input frames than they produce output frames for a single
   * operation, we cannot use the same model as spatial streaming which assumes
   * that the input requested region is fully populated before producing any
   * output. Instead, once we have split the requested output region, we reset
   * the requested temporal region of the input to each input requested
   * temporal sub-region (in sequence) and re-propagate the temporal region
   * request up the pipeline. */
  virtual void GenerateData();

  /** TemporalStreamingGenerateData is in charge of producing output for a
   * single portion of the output requested temporal region. This is where
   * the body of the process will take place. Subclasses that handle spatial
   * data (such as video frames) may instead use this function to split the
   * requested spatial region and process the spatial sub-regions using the
   * mechanism implemented in ImageBase for multithreading. */
  virtual void TemporalStreamingGenerateData(unsigned long outputFrameStart) ITK_NO_RETURN;

protected:

  /*-PROTECTED METHODS-------------------------------------------------------*/

  /** Constructor that initializes members */
  TemporalProcessObject();

  /** Empty Destructor */
  virtual ~TemporalProcessObject(){};

  /** ITK print mechanism */
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Explicitly handle temporal regions in EnlargeRequestedRegion. The default
   * implementation makes sure that the output reuqested temporal region is a
   * multiple of the unit output size. */
  virtual void EnlargeOutputRequestedTemporalRegion(TemporalDataObject* output);

  /** Explicitly handle temporal regions in GeneratOutputRegion. The default
   * implementation does nothing */
  virtual void GenerateOutputRequestedTemporalRegion(TemporalDataObject* output) {};

  /** Explicitly handle temporal regions in GenerateInputRequestedRegion. The
   * default implementation sets the requested temporal region on the input to
   * start at m_FrameSkipPerOutput times the start frame of the requested output
   * temporal region with the duration needed to produce the entire requested
   * output.
   *
   * NOTE: This default propagation will be overwritten during the
   * UpdateOutputData phase by the temporal streaming mechanism if a subclass
   * implements TemporalStreamingGenerateData, but this propagation is provided
   * so that subclasses which directly implement GenerateData will work
   * correctly. */
  virtual void GenerateInputRequestedTemporalRegion();

  /** Split the output's RequestedTemporalRegion into the proper number of
   * sub-regions. By default it is assumed that each sub-region processed
   * should be generated using the set of input frames starting one frame
   * forward in time from the previous sub-region. To change this, set
   * FrameSkipPerOutput to something other than 1. Positive indicates forward
   * in time while negative indicates backward in time.
   * The set of returned TemporalRegion objects is the set of temporal regions
   * that are needed as input to generate the entire output requested region */
  virtual std::vector<TemporalRegion> SplitRequestedTemporalRegion();

  /** Method that gets called by GenerateData before
   * TemporalStreamingGenerateData. Subclasses can override this method to
   * provide pre-processing for the data before splitting up the requested
   * output temporal region. */
  virtual void BeforeTemporalStreamingGenerateData() {};

  /** Method that gets called by GenerateData after
   * TemporalStreamingGenerateData. Subclasses can override this method to
   * provide post-processing for the data after producing the requested output
   * temporal region. */
  virtual void AfterTemporalStreamingGenerateData() {};

  /** Generate a default LargestPossibleRegion. This is used by temporal
   * process objects that have no input. The default implementation starts at
   * frame 0 and has infinite duration. */
  virtual TemporalRegion GenerateDefaultLargestPossibleRegion();


  /*-PROTECTED MEMBERS-------------------------------------------------------*/

  /** Members to indicate the number of input frames and output frames requred
   * to perform a single pass through the processing. */
  unsigned long m_UnitInputNumberOfFrames;
  unsigned long m_UnitOutputNumberOfFrames;

  /** Number of frames to move in order to produce each set of output frames.
   * There is no public API for this member, but subclasses can set it
   * internally (or provide access to it) if they wish. */
  long m_FrameSkipPerOutput;

  /** Member to indicate the location of the "current frame" in the minimum set
   * of input frames. For example, if the unit number of input frames is 3 and
   * m_InputStencilCurrentFrameIndex = 1, this indicates that for output frame
   * n, frames n-1 through n+1 are required, whereas if
   * m_InputStencilCurrentFrameIndex = 0, frames n through n+2 are required. */
  long m_InputStencilCurrentFrameIndex;

private:
  TemporalProcessObject(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented


};  // end class TemporalProcessObject

} // end namespace itk

#endif
