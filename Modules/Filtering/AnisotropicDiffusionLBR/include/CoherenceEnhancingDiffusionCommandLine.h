//
//  CoherenceEnhancingDiffusionCommandLine.h
//  itkDiffusion
//
//  Created by Jean-Marie Mirebeau on 20/11/2014.
//
//

#ifndef itkDiffusion_CoherenceEnhancingDiffusionCommandLine_h
#define itkDiffusion_CoherenceEnhancingDiffusionCommandLine_h

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "CoherenceEnhancingDiffusionFilter.h"
#include "LinearAnisotropicDiffusionCommandLine.h"

namespace CoherenceEnhancingDiffusionCommandLine {

    void Usage(){
        std::cerr <<
        "Input image filename. 2D and 3D images supported. Required.\n" <<
        "Output image filename. Required.\n" <<
        "Diffusion time. Suggested range: 0.5-5, up to 50 for strong noise. Default: 2.\n" <<
        "Lambda. Small values detect more edges. Suggested range: 0.05, 0.0001. Default: 0.01\n" <<
        "Weickert diffusion type. Accepted values: Edge, Coherence. Default: Edge.\n" <<
        "Noise scale. Suggested range: 0.5 - 4. Default 1.\n" <<
        "Feature scale. Suggested range: 2-6. Default 2.\n"
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
    
    
    typedef LinearAnisotropicDiffusionCommandLine::ReportProgressToCErrType ReportProgressToCErrType;    
    
int Execute(int argc, char * argv[])
{
    using std::cerr;
    using std::endl;
    using namespace itk;
    
    if(argc<2+1) {Usage(); return EXIT_SUCCESS ;}
    
    const char *ImageFileName =argv[0+1];
    typedef ImageFileReader<Image<unsigned char,3> > ReaderType;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(ImageFileName);
    
    reader->UpdateOutputInformation();
    
    const ImageIOBase * io = reader->GetImageIO();
    const int ImageDimension = io->GetNumberOfDimensions();
    const itk::ImageIOBase::IOComponentType componentType = io->GetComponentType();
    const int nComponents = io->GetNumberOfComponents();
    
    switch (ImageDimension) {
        case 2: return Execute<2>(argc,argv,componentType,nComponents);
        case 3: return Execute<3>(argc,argv,componentType,nComponents);
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
    
    typedef Image<PixelType,Dimension> ImageType;
    
    typedef ImageFileReader<ImageType> ReaderType;
    typename ReaderType::Pointer reader = ReaderType::New();
    
    const char *imageFileName =argv[0+1];
    const char *outputFileName =argv[1+1];
    reader->SetFileName(imageFileName);
    
    /*{
        reader->Update();
        auto image = reader->GetOutput();
        std::cerr << image->GetRequestedRegion() << "\n\n";
        std::cerr << image->GetRequestedRegion() << "\n\n";
    }*/
    
    typedef CoherenceEnhancingDiffusionFilter<ImageType,ScalarType> DiffusionFilterType;
    typename DiffusionFilterType::Pointer diffusionFilter = DiffusionFilterType::New();
    diffusionFilter->SetInput(reader->GetOutput());
    
    ReportProgressToCErrType::Pointer reportDiffusionProgress = ReportProgressToCErrType::New();
    diffusionFilter->AddObserver(ProgressEvent(), reportDiffusionProgress);
    
    int argIndex = 3;
    if(argIndex<argc){
        const double diffusionTime = atof(argv[argIndex++]);
        if(diffusionTime==0) itkGenericExceptionMacro("Error: Unrecognized diffusion time (third argument).\n");
        diffusionFilter->SetDiffusionTime(diffusionTime);
    }
    
    if(argIndex<argc){
        const double lambda = atof(argv[argIndex++]);
        if(lambda==0.) itkGenericExceptionMacro("Error: Unrecognized lambda (fourth argument).\n");
        diffusionFilter->SetLambda(lambda);
    }
    
    if(argIndex<argc){
        const char * enhancement = argv[argIndex++];
        if(!strcmp(enhancement,"EED"))
            diffusionFilter->SetEnhancement(DiffusionFilterType::EED); // Weickert's exponent : 4.
        else if(!strcmp(enhancement,"cEED"))
            diffusionFilter->SetEnhancement(DiffusionFilterType::cEED); // Weickert's exponent : 4.
        else if(!strcmp(enhancement,"CED"))
            diffusionFilter->SetEnhancement(DiffusionFilterType::CED); // Weickert's exponent : 2.
        else if(!strcmp(enhancement, "cCED"))
            diffusionFilter->SetEnhancement(DiffusionFilterType::cCED); // Weickert's exponent : 2.
        else if(!strcmp(enhancement, "Isotropic"))
            diffusionFilter->SetEnhancement(DiffusionFilterType::Isotropic); //Perona-Mali's exponent: 2.
        else
            itkGenericExceptionMacro("Error: Unrecognized enhancement (fifth argument).\n");
    }
    
    if(argIndex<argc){
        const double noiseScale = atof(argv[argIndex++]);
        if(noiseScale==0.) itkGenericExceptionMacro("Error: Unrecognized noiseScale (sixth argument).\n");
        diffusionFilter->SetNoiseScale(noiseScale);
    }

    if(argIndex<argc){
        const double featureScale = atof(argv[argIndex++]);
        if(featureScale==0.) itkGenericExceptionMacro("Error: Unrecognized featureScale (seventh argument).\n");
        diffusionFilter->SetFeatureScale(featureScale);
    }
    
    if(argIndex<argc){
        const double exponent = atof(argv[argIndex++]);
        if(exponent==0.) itkGenericExceptionMacro("Error: Unrecognized exponent (eighth argument).\n");
        diffusionFilter->SetExponent(exponent);
    }
    
    if(argIndex<argc){
        itkGenericExceptionMacro("Error: excessive number of arguments");
    }
    
    
    /*{
        std::cerr <<
        "T: " << diffusionFilter->GetDiffusionTime() << "\n" <<
        "Lambda: " << diffusionFilter->GetLambda() << "\n" <<
        "argc: " << argc << "\n";

        diffusionFilter->Update();
        auto image = diffusionFilter->GetOutput();
        std::cerr <<
        image->GetBufferedRegion() << "\n\n" <<
        "pixel export size: " << sizeof(ExportPixelType) << "\n";
        ;

    }*/
    

    typedef Image<ExportPixelType,Dimension> ExportImageType;
    typedef CastImageFilter<ImageType, ExportImageType> CasterType;
    typename CasterType::Pointer caster = CasterType::New();
    caster->SetInput(diffusionFilter->GetOutput());
    
    //typedef typename DiffusionFilterType::ScalarImageType ScalarImageType;
    typedef ImageFileWriter<ExportImageType> WriterType;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetInput(caster->GetOutput());
    writer->SetFileName(outputFileName);
    
    clock_t top=-clock();
    writer->Update();
    top+=clock(); std::cout << "Took : " << top/double(CLOCKS_PER_SEC) << "s\n";
    
    return EXIT_SUCCESS;
}
    
    
    
}

#endif
