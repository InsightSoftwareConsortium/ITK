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
//
//  Created by Jean-Marie Mirebeau on 01/12/2014.
//
//

#ifndef itkLinearAnisotropicDiffusionCommandLine_h
#define itkLinearAnisotropicDiffusionCommandLine_h

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCoherenceEnhancingDiffusionImageFilter.h"

namespace LinearAnisotropicDiffusionCommandLine
{

void Usage(){
    std::cerr <<
    "Input image filename. 2D and 3D images supported. Required.\n" <<
    "Output tensor field filename. Required.\n" <<
    "Diffusion time. Required.\n" <<
    "Output image filename. Required.\n" <<
    "RatioToMaxStableStep. Range: ]0,1]. Default: 0.7. Optionnal.\n" <<
    "MaxNumberOfIterations. Range: 1...Infinity. Default: 200. Optionnal.\n"
    << "\n";
}

using namespace itk;

int Execute(int argc, char * argv[]);

template<int VDimension>
int Execute(int argc, char * argv[], itk::ImageIOBase::IOComponentType, int nComponents);

template<int VDimension, typename ScalarType, typename ComponentType>
int Execute(int argc, char * argv[], int nComponents);

template<int VDimension, typename ScalarType, typename PixelType, typename ExportPixelType>
int Execute(int argc, char * argv[]);


struct ReportProgressToCOutType : public itk::Command
{
    itkNewMacro(ReportProgressToCOutType);
    void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE{
        Execute( (const itk::Object *)caller, event);
    }

    void Execute(const itk::Object * object, const itk::EventObject &) ITK_OVERRIDE{
        std::cout << object->GetNameOfClass() << " has completed: "
        << int(100*dynamic_cast<const itk::ProcessObject*>(object)->GetProgress())
        << "%" << std::endl;
    }
};


int Execute(int argc, char * argv[])
{
    using std::cerr;
    using std::endl;
    using namespace itk;

    if(argc<4+1) {Usage(); return EXIT_SUCCESS ;}

    const char *imageFileName =argv[0+1];
    typedef ImageFileReader<Image<unsigned char,3> > ReaderType;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(imageFileName);

    reader->UpdateOutputInformation();

    const ImageIOBase * io = reader->GetImageIO();
    const int ImageDimension = io->GetNumberOfDimensions();
    const itk::ImageIOBase::IOComponentType componentType = io->GetComponentType();
    const int nComponents = io->GetNumberOfComponents();

    {
        const char *tensorFileName = argv[1+1];
        ReaderType::Pointer reader2 = ReaderType::New();
        reader2->SetFileName(tensorFileName);
        reader2->UpdateOutputInformation();
        const ImageIOBase * io2 = reader2->GetImageIO();
        if(io2->GetComponentType() != componentType)
            std::cerr << "Warning: image and tensors have distinct component types.\n";
        if(ImageDimension != (int)io2->GetNumberOfDimensions())
            itkGenericExceptionMacro("Error: image and tensor image have distinct dimension");
        const int TensorDimension = (ImageDimension*(ImageDimension+1))/2;
        if(TensorDimension != (int)io2->GetNumberOfComponents())
            itkGenericExceptionMacro("Error: wrong tensor dimension, should be n*(n+1)/2 where n=ImageDimension.");
        if(io2->GetPixelType() == itk::ImageIOBase::SYMMETRICSECONDRANKTENSOR)
            std::cerr << "Warning: tensor image pixel type not marked as Symmetric Second Rank Tensor.\n";
    }

    switch (ImageDimension) {
        case 2: return Execute<2>(argc,argv,componentType,nComponents);
//        case 3: return Execute<3>(argc,argv,componentType,nComponents);
        default: itkGenericExceptionMacro("Sorry, unsupported image dimension.");
    }
}

template<int Dimension>
int Execute(int argc, char * argv[], itk::ImageIOBase::IOComponentType componentType, int nComponents){
    switch (componentType) {
        case itk::ImageIOBase::UCHAR: return Execute<Dimension, float, unsigned char>(argc,argv,nComponents);
        case itk::ImageIOBase::FLOAT: return Execute<Dimension, float, float>(argc,argv,nComponents);
        case itk::ImageIOBase::DOUBLE:return Execute<Dimension, double, double>(argc,argv,nComponents);
        default: itkGenericExceptionMacro("Sorry, unsupported component type");
    }
}

template<int Dimension, typename ScalarType, typename ComponentType>
int Execute(int argc, char * argv[], int nComponents){
    switch (nComponents) {
        case 1: return Execute<Dimension,ScalarType,ScalarType,ComponentType>(argc,argv);
        case 2: return Execute<Dimension,ScalarType,Vector<ScalarType,2>,Vector<ComponentType,2> >(argc,argv);
        case 3: return Execute<Dimension,ScalarType,Vector<ScalarType,3>,Vector<ComponentType,3> >(argc,argv);
        default: itkGenericExceptionMacro("Sorry, unsupported number of components");
    }
}

template<int Dimension, typename ScalarType, typename PixelType, typename ExportPixelType>
int Execute(int argc, char * argv[]){

    // Import image
    typedef Image<PixelType,Dimension> ImageType;
    typedef ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer reader = ReaderType::New();
    const char *imageFileName =argv[0+1];
    reader->SetFileName(imageFileName);

    // Import tensors
    typedef SymmetricSecondRankTensor<ScalarType,Dimension> TensorType;
    typedef Image<TensorType,Dimension> TensorImageType;
    typedef ImageFileReader<TensorImageType> TensorReaderType;
    typename TensorReaderType::Pointer tensorReader = TensorReaderType::New();
    const char * tensorImageFileName = argv[1+1];
    tensorReader->SetFileName(tensorImageFileName);

    // Import diffusion time
    const double diffusionTime = atof(argv[2+1]);
    if(diffusionTime==0) itkGenericExceptionMacro("Error: Unrecognized diffusion time (third argument).\n");

    // Import output image filename
    const char *outputFileName = argv[3+1];

    // Setup diffusion filter
    typedef LinearAnisotropicDiffusionLBRImageFilter<ImageType,ScalarType> DiffusionFilterType;
    typename DiffusionFilterType::Pointer diffusionFilter = DiffusionFilterType::New();
    diffusionFilter->SetInputImage(reader->GetOutput());
    diffusionFilter->SetInputTensor(tensorReader->GetOutput());
    diffusionFilter->SetMaxDiffusionTime(diffusionTime);

    int argIndex = 4+1;
    if(argIndex<argc){
        const double ratioToMaxStableTimeStep = atof(argv[argIndex++]);
        if(ratioToMaxStableTimeStep==0) itkGenericExceptionMacro("Error: Unrecognized RatioToMaxStableTimeStep (fourth argument).\n");
        diffusionFilter->SetRatioToMaxStableTimeStep(ratioToMaxStableTimeStep);
    }

    if(argIndex<argc){
        const int maxNumberOfTimeSteps = atoi(argv[argIndex++]);
        if(maxNumberOfTimeSteps==0) itkGenericExceptionMacro("Error: Unrecognized maxNumberOfTimeSteps (fifth argument).\n");
        diffusionFilter->SetMaxNumberOfTimeSteps(maxNumberOfTimeSteps);
    } else
        diffusionFilter->SetMaxNumberOfTimeSteps(200);

    ReportProgressToCOutType::Pointer reportDiffusionProgress = ReportProgressToCOutType::New();
    diffusionFilter->AddObserver(ProgressEvent(), reportDiffusionProgress);

    typedef Image<ExportPixelType,Dimension> ExportImageType;
    typedef CastImageFilter<ImageType, ExportImageType> CasterType;
    typename CasterType::Pointer caster = CasterType::New();
    caster->SetInput(diffusionFilter->GetOutput());

    //typedef typename DiffusionFilterType::ScalarImageType ScalarImageType;
    typedef ImageFileWriter<ExportImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput(caster->GetOutput());
    writer->SetFileName(outputFileName);
    writer->Update();

    const ScalarType effectiveDiffusionTime=diffusionFilter->GetEffectiveDiffusionTime();
    if(effectiveDiffusionTime < 0.99 * diffusionTime){
        std::cerr <<
        "Warning: early abort at effective diffusion time: " << effectiveDiffusionTime <<
        ", you may want to increase the max number of time steps: " << diffusionFilter->GetMaxNumberOfTimeSteps() << "\n";
        Usage();
    }

    return EXIT_SUCCESS;
}

} // end namespace LinearAnisotropicDiffusionCommandLine

#endif
