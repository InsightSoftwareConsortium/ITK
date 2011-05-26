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
#ifndef __itkPipelineMonitorImageFilter_h
#define __itkPipelineMonitorImageFilter_h

#include "itkInPlaceImageFilter.h"

namespace itk {

  // this class is usefull for verifying streaming and requested regions
 template <class TImageType>
   class PipelineMonitorImageFilter :
    public InPlaceImageFilter< TImageType, TImageType>
 {
 public:
   typedef PipelineMonitorImageFilter                  Self;
   typedef InPlaceImageFilter<TImageType, TImageType>  Superclass;
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
   itkTypeMacro(PipelineMonitorImageFilter,ImageToImageFilter);

   /** Enable/Disable clearing all saved pipeline information when
   * GenerateOutputInformation is called.
   *
   * Defaults to On
   */
   itkSetMacro( ClearPipelineOnGenerateOutputInformation, bool );
   itkGetMacro( ClearPipelineOnGenerateOutputInformation, bool );
   itkBooleanMacro( ClearPipelineOnGenerateOutputInformation );

   bool VerifyDownStreamFilterExecutedPropagation(void)
   {
     bool ret = true;
     // we expect that the propagation is not going to be called
     // extra times
     if (m_OutputRequestedRegions.size() != this->GetNumberOfUpdates() ||
         m_InputRequestedRegions.size() != this->GetNumberOfUpdates())
       {
       itkWarningMacro(<<"Down stream filter didn't execute PropagateRequestedRegion well");
       ret = false;
       }
     return ret;
   }

   bool VerifyInputFilterExecutedStreaming(int expectedNumber)
   {
     if (expectedNumber == 0)
       {
       return true;
       }
     else if (expectedNumber < 0 && static_cast<unsigned int>(- expectedNumber) <=  this->GetNumberOfUpdates())
       {
       return true;
       }
     else if ( expectedNumber == static_cast<int>(this->GetNumberOfUpdates()) )
       {
       return true;
       }
     itkWarningMacro(<<"Streamed pipeline was executed " << this->GetNumberOfUpdates()
                     << " times which was not the expected number "  << expectedNumber
                     << " of times.");
     return false;
   }

   bool VerifyInputFilterMatchedUpdateOutputInformation(void)
   {
     InputImageConstPointer input =  this->GetInput();
     if (input->GetSpacing() != m_UpdatedOutputSpacing)
       {
       itkWarningMacro(<<"The input filter's Spacing does not match UpdateOutputInformation");
       return false;
       }
     if (input->GetOrigin() != m_UpdatedOutputOrigin)
       {
       itkWarningMacro(<<"The input filter's Origin does not match UpdateOutputInformation");
       return false;
       }
     if (input->GetDirection() != m_UpdatedOutputDirection)
       {
       itkWarningMacro(<<"The input filter's Direction does not match UpdateOutputInformation");
       return false;
       }
     if(input->GetLargestPossibleRegion() != m_UpdatedOutputLargestPossibleRegion)
       {
       itkWarningMacro(<<"The input filter's LargestPossibleRegion does not match UpdateOutputInformation");
       return false;
       }
     if(m_UpdatedBufferedRegions.size() && !m_UpdatedOutputLargestPossibleRegion.IsInside(m_UpdatedBufferedRegions.back()))
       {
       itkWarningMacro(<<"The input filter's BufferedRegion is not contained by LargestPossibleRegion");
       return false;
       }
       return true;
   }


   bool VerifyInputFilterBufferedRequestedRegions(void)
   {
     // we expect that the input filter's output image's buffered
     // region is going to match it's requested region
     bool ret = true;
     unsigned int i;
     for (i = 0; i < m_UpdatedBufferedRegions.size(); ++i)
       {
       if (m_UpdatedBufferedRegions[i] != m_UpdatedRequestedRegions[i])
         {
         itkWarningMacro(<<"The input filter's updated buffered region was not the requested region");
         ret =  false;
         }
       }
     return ret;
   }

   bool VerifyInputFilterMatchedRequestedRegions(void)
   {
     // we expect that the input filter's output image's buffered
     // region is going to match it's requested region, which is going
     // to match the requested region at the end of propagation
     //
     bool ret = true;
     unsigned int i = m_UpdatedBufferedRegions.size();
     unsigned int j = m_InputRequestedRegions.size();

     while(i != 0 && j != 0)
       {
       if (m_UpdatedBufferedRegions[--i] != m_InputRequestedRegions[--j])
         {
         itkWarningMacro(<<"The input filter's updated buffer region was not the region we requested");
         ret =  false;
         }
       }

     return ret;
   }

   bool VerifyInputFilterRequestedLargestRegion(void)
   {
     if (m_InputRequestedRegions.back() != m_UpdatedOutputLargestPossibleRegion)
       {
       itkWarningMacro(<<"The input filter didn't set it's output request to the largest region");
       return false;
       }
     return true;
   }

   bool VerifyAllInputCanStream(int expectedNumber)
   {
     return VerifyInputFilterExecutedStreaming(expectedNumber) &&
       VerifyDownStreamFilterExecutedPropagation() &&
       VerifyInputFilterMatchedRequestedRegions() &&
       VerifyInputFilterBufferedRequestedRegions() &&
       VerifyInputFilterMatchedUpdateOutputInformation();
   }

   bool VerifyAllInputCanNotStream(void)
   {
     return VerifyDownStreamFilterExecutedPropagation() &&
       VerifyInputFilterRequestedLargestRegion() &&
       VerifyInputFilterBufferedRequestedRegions() &&
       VerifyInputFilterMatchedUpdateOutputInformation();
   }

   bool VerifyAllNoUpdate(void)
   {
     return VerifyDownStreamFilterExecutedPropagation() &&
       m_NumberOfUpdates == 0;
   }


   unsigned int GetNumberOfUpdates(void) const { return m_NumberOfUpdates; }
   RegionVectorType GetOutputRequestedRegions(void) const {return m_OutputRequestedRegions;}
   RegionVectorType GetInputRequestedRegions(void) const {return m_InputRequestedRegions;}
   RegionVectorType GetUpdatedBufferedRegions(void) const {return m_UpdatedBufferedRegions; }
   RegionVectorType GetUpdatedRequestedRegions(void) const {return m_UpdatedRequestedRegions; }


   void ClearPipelineSavedInformation(void)
   {
     m_NumberOfUpdates = 0;
     m_OutputRequestedRegions.clear();
     m_InputRequestedRegions.clear();
     m_UpdatedBufferedRegions.clear();
     m_UpdatedRequestedRegions.clear();

     m_UpdatedOutputOrigin.Fill(-1);
     m_UpdatedOutputDirection.Fill(-1);
     m_UpdatedOutputSpacing.Fill(-1);

     ++m_NumberOfClearPipeline;
   }

   virtual void GenerateOutputInformation(void)
   {
     if ( m_ClearPipelineOnGenerateOutputInformation )
       {
       this->ClearPipelineSavedInformation();
       }

     Superclass::GenerateOutputInformation();

     InputImageConstPointer  input  = this->GetInput();
     m_UpdatedOutputOrigin = input->GetOrigin();
     m_UpdatedOutputDirection = input->GetDirection();
     m_UpdatedOutputSpacing = input->GetSpacing();
     m_UpdatedOutputLargestPossibleRegion = input->GetLargestPossibleRegion();
     itkDebugMacro("GenerateOutputInformation called");
   }

   virtual void PropagateRequestedRegion(DataObject *output)
   {
     // call the superclass' implementation of this method
     Superclass::PropagateRequestedRegion(output);

     // record the regions after everything has executed
     itkDebugMacro("After PropagateRequestedRegion: " << this->GetInput()->GetRequestedRegion());
     this->m_InputRequestedRegions.push_back(this->GetInput()->GetRequestedRegion());
     this->m_OutputRequestedRegions.push_back(this->GetOutput()->GetRequestedRegion());
   }

   virtual void EnlargeOutputRequestedRegion( DataObject *output)
   {
     // call the superclass' implementation of this method
     Superclass::EnlargeOutputRequestedRegion(output);

     itkDebugMacro("EnlargeOutputRequestRegion: " << this->GetOutput()->GetRequestedRegion());
   }

   virtual void GenerateInputRequestedRegion(void)
   {
     // call the superclass' implementation of this method
     Superclass::GenerateInputRequestedRegion();

     // this is not very interesting as it's always going to be the
     // output requested region
     itkDebugMacro("GenerateInputRequestRegion: " << this->GetInput()->GetRequestedRegion());
   }


    virtual void GenerateData(void)
    {
      // Get pointers to the input and output
      InputImagePointer output = this->GetOutput();
      InputImagePointer input =
        const_cast< TImageType * >( this->GetInput());

      // because this is in place filter this will acutall copy the
      // pixel containers
      this->AllocateOutputs();

      itkDebugMacro("GenerateData Buffered: " << this->GetInput()->GetBufferedRegion() << " Requested:" << this->GetInput()->GetRequestedRegion());

      m_UpdatedBufferedRegions.push_back(this->GetInput()->GetBufferedRegion());
      m_UpdatedRequestedRegions.push_back(this->GetInput()->GetRequestedRegion());

      ++m_NumberOfUpdates;
    }

 protected:
   PipelineMonitorImageFilter(void)
   {
     m_NumberOfClearPipeline = 0;
     m_ClearPipelineOnGenerateOutputInformation = true;
     this->ClearPipelineSavedInformation();
   }

   ~PipelineMonitorImageFilter()
   {
   }

   void PrintSelf(std::ostream &os, Indent indent) const
   {
     Superclass::PrintSelf(os,indent);
     os << indent << "m_NumberOfUpdates: " << m_NumberOfUpdates << std::endl;

     os << indent << "m_NumberOfClearPipeline: " << m_NumberOfClearPipeline << std::endl;

     os << indent << "m_ClearPipelineOnGenerateOutputInformation: " << m_ClearPipelineOnGenerateOutputInformation << std::endl;

     os << indent << "m_OutputRequestedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_OutputRequestedRegions.begin(); i != m_OutputRequestedRegions.end(); ++i)
       i->Print(os, indent.GetNextIndent());

     os << indent << "m_InputRequestedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_InputRequestedRegions.begin(); i != m_InputRequestedRegions.end(); ++i)
       i->Print(os, indent.GetNextIndent());

     os << indent << "m_UpdatedBufferedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_UpdatedBufferedRegions.begin(); i != m_UpdatedBufferedRegions.end(); ++i)
       i->Print(os, indent.GetNextIndent());

     os << indent << "m_UpdatedRequestedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_UpdatedRequestedRegions.begin(); i != m_UpdatedRequestedRegions.end(); ++i)
       i->Print(os, indent.GetNextIndent());

     os << indent << "m_UpdatedOutputOrigin:" << std::endl;
     os << indent.GetNextIndent() << m_UpdatedOutputOrigin << std::endl;
     os << indent << "m_UpdatedOutputDirection:" << std::endl;
     os << indent.GetNextIndent() << m_UpdatedOutputDirection << std::endl;
     os << indent << "m_UpdatedOutputSpacing:" << std::endl;
     os << indent.GetNextIndent() << m_UpdatedOutputSpacing << std::endl;
     os << indent << "m_UpdatedOutputLargestPossibleRegion: " << std::endl;
     m_UpdatedOutputLargestPossibleRegion.Print(os, indent.GetNextIndent());
   }

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

}

#endif
