#ifndef __ConnectVTKITK_h
#define __ConnectVTKITK_h

template <typename ImageType>
void ConnectITKToVTK(itk::VTKImageExport<ImageType > *in, 
                     vtkImageImport* out ) 
{
  out->SetUpdateInformationCallback(in->GetUpdateInformationCallback());
  out->SetPipelineModifiedCallback(in->GetPipelineModifiedCallback());
  out->SetWholeExtentCallback(in->GetWholeExtentCallback());
  out->SetSpacingCallback(in->GetSpacingCallback());
  out->SetOriginCallback(in->GetOriginCallback());
  out->SetScalarTypeCallback(in->GetScalarTypeCallback());
  out->SetNumberOfComponentsCallback(in->GetNumberOfComponentsCallback());
  out->SetPropagateUpdateExtentCallback(in->GetPropagateUpdateExtentCallback());
  out->SetUpdateDataCallback(in->GetUpdateDataCallback());
  out->SetDataExtentCallback(in->GetDataExtentCallback());
  out->SetBufferPointerCallback(in->GetBufferPointerCallback());
  out->SetCallbackUserData(in->GetCallbackUserData());
};


template <typename ImageType>
void ConnectVTKToITK (vtkImageExport* in, 
                      itk::VTKImageImport<ImageType > *out ) 
{
  out->SetUpdateInformationCallback(in->GetUpdateInformationCallback());
  out->SetPipelineModifiedCallback(in->GetPipelineModifiedCallback());
  out->SetWholeExtentCallback(in->GetWholeExtentCallback());
  out->SetSpacingCallback(in->GetSpacingCallback());
  out->SetOriginCallback(in->GetOriginCallback());
  out->SetScalarTypeCallback(in->GetScalarTypeCallback());
  out->SetNumberOfComponentsCallback(in->GetNumberOfComponentsCallback());
  out->SetPropagateUpdateExtentCallback(in->GetPropagateUpdateExtentCallback());
  out->SetUpdateDataCallback(in->GetUpdateDataCallback());
  out->SetDataExtentCallback(in->GetDataExtentCallback());
  out->SetBufferPointerCallback(in->GetBufferPointerCallback());
  out->SetCallbackUserData(in->GetCallbackUserData());
};

#endif
