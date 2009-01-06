#include "itkImageToImageFilter.h"

namespace itk {

  // this class is usefull for verifying streaming and requested regions
 template <class TImageType>
   class PipelineMonitorImageFilter :
    public ImageToImageFilter< TImageType, TImageType> 
 {
 public:
   typedef PipelineMonitorImageFilter Self;
   typedef ImageToImageFilter<TImageType, TImageType>  Superclass;
   typedef SmartPointer<Self>  Pointer;
   typedef SmartPointer<const Self>  ConstPointer;
   
   
   typedef typename TImageType::Pointer InputImagePointer;
   typedef std::vector<typename TImageType::RegionType> RegionVectorType;

   /** Method for creation through the object factory. */
   itkNewMacro(Self);
   
   /** Run-time type information (and related methods). */
   itkTypeMacro(PipelineMonitorImageFilter,ImageToImageFilter);


   unsigned int GetNumberOfUpdates(void) const { return m_NumberOfUpdates; }      
   RegionVectorType GetOutputRequestedRegions(void) const {return m_OutputRequestedRegions;}
   RegionVectorType GetInputRequestedRegions(void) const {return m_InputRequestedRegions;}
   RegionVectorType GetUpdatedRegions(void) const {return m_UpdatedRegions; }

   
   void ClearPipelineSavedInformation(void) {
     m_NumberOfUpdates = 0;
     m_OutputRequestedRegions.clear();
     m_InputRequestedRegions.clear();
     m_UpdatedRegions.clear();
   }


   virtual void EnlargeOutputRequestedRegion( DataObject *output) {
     // call the superclass' implementation of this method
     Superclass::EnlargeOutputRequestedRegion(output);

     this->m_OutputRequestedRegions.push_back(this->GetOutput()->GetRequestedRegion());
     itkDebugMacro("EnlargeOutputRequestRegion: " << this->GetOutput()->GetRequestedRegion());
   }

   virtual void GenerateInputRequestedRegion(void) {
     
     // call the superclass' implementation of this method
     Superclass::GenerateInputRequestedRegion();

     this->m_InputRequestedRegions.push_back(this->GetInput()->GetRequestedRegion());
     itkDebugMacro("GenerateInputRequestRegion: " << this->GetInput()->GetRequestedRegion());
     
   }
   
   virtual void GenerateData(void) {
     // Get pointers to the input and output
     InputImagePointer output = this->GetOutput();
     InputImagePointer input = 
       const_cast< TImageType * >( this->GetInput());
     
     // No need to copy the bulk data
     // however this may not be the requested region on the output
     output->SetPixelContainer(input->GetPixelContainer());
     output->SetBufferedRegion(this->GetInput()->GetBufferedRegion());

     m_UpdatedRegions.push_back(this->GetInput()->GetBufferedRegion());

     ++m_NumberOfUpdates;
   }

 protected:
   PipelineMonitorImageFilter(void) {
     m_NumberOfUpdates = 0;
   }

   ~PipelineMonitorImageFilter() {
   }

   void PrintSelf(std::ostream &os, Indent indent) const {     
     Superclass::PrintSelf(os,indent);
     os << indent << "m_NumberOfUpdates: " << m_NumberOfUpdates << std::endl;
     os << indent << "m_OutputRequestedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_OutputRequestedRegions.begin(); i != m_OutputRequestedRegions.end(); ++i) 
       i->Print(os, indent.GetNextIndent());
     os << indent << "m_InputRequestedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_InputRequestedRegions.begin(); i != m_InputRequestedRegions.end(); ++i) 
       i->Print(os, indent.GetNextIndent());     
     os << indent << "m_UpdatedRegions:"<< std::endl;
     for (typename RegionVectorType::const_iterator i = m_UpdatedRegions.begin(); i != m_UpdatedRegions.end(); ++i) 
       i->Print(os, indent.GetNextIndent());
   }

 private:
   PipelineMonitorImageFilter(const PipelineMonitorImageFilter &);
   void operator=(const PipelineMonitorImageFilter &);

   unsigned int m_NumberOfUpdates;
   RegionVectorType m_OutputRequestedRegions;
   RegionVectorType m_InputRequestedRegions;
   RegionVectorType m_UpdatedRegions;

 };

}
