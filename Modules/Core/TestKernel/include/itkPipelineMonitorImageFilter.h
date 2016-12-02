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
#ifndef itkPipelineMonitorImageFilter_h
#define itkPipelineMonitorImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class PipelineMonitorImageFilter
 *
 * \brief Enables monitoring, recording and debugging of the pipeline
 * execution and information exchange.
 *
 * This filter is useful for testing, debugging, and understanding the
 * pipeline. When DebugOn is enabled and compiled in Debug mode, many
 * itkDebug messages are printed. This filter also features, several
 * Verify methods which check the recorded information, for certain
 * conditions, which should occur when well behaved filters are
 * executed.
 *
 * There are two meta verify methods that should primarily be used
 * depending on the expected capabilities of the pipeline:
 * - \b VerifyAllInputCanStream
 * - \b VerifyAllInputCanNotStream
 *
 * During the pipeline execution this filter records a variety of
 * information to aid if verifying correct pipeline behavior:
 * - \b NumberOfUpdate the number of times GenerateData was executed
 * - \b OutputRequestedRegions an array of the output of this filter's
 *      requested region
 * - \b InputRequestedRegions an array of the input image's requested
 *      region after PropagateRequestedRegion
 * - \b UpdatedBufferedRegions an array of the input image's buffered
 *      region after upstream GenerateData
 * - \b UpdatedRequestedRegions an array of the input image's
 *      requested region after upstream GenerateData
 *
 * The following are recorded from the input image after the input's
 * output information is generated:
 * - \b UpdatedOutputOrigin
 * - \b UpdatedOutputDirection
 * - \b UpdatedOutputSpacing
 * - \b UpdatedOutputLargestPossibleRegion
 *
 * This filter always runs in-place so it has no per-pixel overhead.
 *
 * \ingroup ITKTestKernel
 */
 template <typename TImageType>
 class ITK_TEMPLATE_EXPORT PipelineMonitorImageFilter :
  public ImageToImageFilter< TImageType, TImageType>
 {
 public:

   typedef PipelineMonitorImageFilter                  Self;
   typedef ImageToImageFilter<TImageType, TImageType>  Superclass;
   typedef SmartPointer<Self>                          Pointer;
   typedef SmartPointer<const Self>                    ConstPointer;

   typedef typename TImageType::PointType             PointType;
   typedef typename TImageType::DirectionType         DirectionType;
   typedef typename TImageType::SpacingType           SpacingType;
   typedef typename TImageType::Pointer               InputImagePointer;
   typedef typename TImageType::ConstPointer          InputImageConstPointer;
   typedef typename Superclass::InputImageRegionType  ImageRegionType;

   typedef std::vector<typename TImageType::RegionType> RegionVectorType;

   /** Method for creation through the object factory. */
   itkNewMacro(Self);

   /** Run-time type information (and related methods). */
   itkTypeMacro(PipelineMonitorImageFilter,InPlaceImageFilter);

   /** Enable/Disable clearing all saved pipeline information when
    * GenerateOutputInformation is called.
    *
    * The NumberOfClearPipelines is incremented, to aid in detection
    * of certain pipeline errors caused but excessive execution of
    * GenerateOutputInformation.
    *
    * Defaults to On
    */
   itkSetMacro( ClearPipelineOnGenerateOutputInformation, bool );
   itkGetMacro( ClearPipelineOnGenerateOutputInformation, bool );
   itkBooleanMacro( ClearPipelineOnGenerateOutputInformation );


   /** This a meta verify method to check expected pipeline execution
    * when the pipeline is capable of streaming. See
    * VerifyInputFilterExecutedStreaming for information on the
    * expectedNumber parameter.
    */
   bool VerifyAllInputCanStream(int expectedNumber);


   /** Checks that the input filter didn't stream, and just updated
    * the largest possible region along with other correct behaviors.
    */
   bool VerifyAllInputCanNotStream();

   /** This method verifies that propagation was executed yet no
    * updating was needed.
    */
   bool VerifyAllNoUpdate();

   bool VerifyDownStreamFilterExecutedPropagation();

   /** Verifies the the GenerateData executed the expected number of
    * times.
    *
    * If expecetedNumber is positive then the number of updates must
    * match. If expectedNumber is negative then the number of updates
    * must at least be |expectedNumber|. If expectedNumber is zero,
    * then this method always returns true, and no verification is
    * performed.
    */
   bool VerifyInputFilterExecutedStreaming(int expectedNumber);

   /** Verifies that the output information didn't change between the
    * GenerateOutputInformation and the UpdateData phases of the
    * pipeline.
    */
   bool VerifyInputFilterMatchedUpdateOutputInformation();

   /** Verifies that the input filter buffered the requested region */
   bool VerifyInputFilterBufferedRequestedRegions();

   bool VerifyInputFilterMatchedRequestedRegions();

   bool VerifyInputFilterRequestedLargestRegion();


   unsigned int GetNumberOfUpdates(void) const { return m_NumberOfUpdates; }
   RegionVectorType GetOutputRequestedRegions(void) const {return m_OutputRequestedRegions;}
   RegionVectorType GetInputRequestedRegions(void) const {return m_InputRequestedRegions;}
   RegionVectorType GetUpdatedBufferedRegions(void) const {return m_UpdatedBufferedRegions; }
   RegionVectorType GetUpdatedRequestedRegions(void) const {return m_UpdatedRequestedRegions; }

   itkGetConstMacro(UpdatedOutputOrigin, PointType);
   itkGetConstMacro(UpdatedOutputDirection, DirectionType);
   itkGetConstMacro(UpdatedOutputSpacing, SpacingType);
   itkGetConstMacro(UpdatedOutputLargestPossibleRegion, ImageRegionType);

   /** Clears all saved pipeline information, but increments
    * NumberOfClearPipeline. */
   void ClearPipelineSavedInformation();


   /** Standard pipeline methods are overloaded to call superclass's
    * implementation and record information.
    */
   virtual void GenerateOutputInformation() ITK_OVERRIDE;
   virtual void PropagateRequestedRegion(DataObject *output) ITK_OVERRIDE;
   virtual void EnlargeOutputRequestedRegion( DataObject *output) ITK_OVERRIDE;
   virtual void GenerateInputRequestedRegion(void) ITK_OVERRIDE;
   virtual void GenerateData(void) ITK_OVERRIDE;

 protected:

   PipelineMonitorImageFilter();

   // ~PipelineMonitorImageFilter() { } default implementation OK

   void PrintSelf(std::ostream &os, Indent indent) const ITK_OVERRIDE;
 private:

   PipelineMonitorImageFilter(const PipelineMonitorImageFilter &); // not implemented
   void operator=(const PipelineMonitorImageFilter &); // not implemented

   bool m_ClearPipelineOnGenerateOutputInformation;

   unsigned int m_NumberOfUpdates;

   unsigned int m_NumberOfClearPipeline;

   RegionVectorType m_OutputRequestedRegions;
   RegionVectorType m_InputRequestedRegions;
   RegionVectorType m_UpdatedBufferedRegions;
   RegionVectorType m_UpdatedRequestedRegions;

   PointType       m_UpdatedOutputOrigin;
   DirectionType   m_UpdatedOutputDirection;
   SpacingType     m_UpdatedOutputSpacing;
   ImageRegionType m_UpdatedOutputLargestPossibleRegion;
 };

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPipelineMonitorImageFilter.hxx"
#endif

#endif //itkPipelineMonitorImageFilter_hxx
