#include "tclap/CmdLine.h"
#include "ioutils.h"

#include <itkMaskImageFilter.h>
#include <itkMaskNegatedImageFilter.h>
// #include <itkSquareImageFilter.h>
// #include <itkBinaryThresholdImageFilter.h>
#include <itkDivideByConstantImageFilter.h>

// #include <itkParabolicDilateImageFilter.h>

#include "itkSpatVarDilateImageFilter.h"
#include "itkSpatVarErodeImageFilter.h"

// Aidan's trick
#include <itkSmartPointer.h>
namespace itk
{
    template <typename T>
    class Instance : public T::Pointer {
    public:
        Instance() : SmartPointer<T>( T::New() ) {}
    };
}



typedef class CmdLineType
{
public:
  std::string InputIm, OutputIm, SizeIm;
};

void ParseCmdLine(int argc, char* argv[],
                  CmdLineType &CmdLineObj
                  )
{
  using namespace TCLAP;
  try
    {
    // Define the command line object.
    CmdLine cmd("varSize ", ' ', "0.9");

    ValueArg<std::string> inArg("i","input","input image (binary mask)",true,"result","string");
    cmd.add( inArg );

    ValueArg<std::string> sizeArg("s","sizefield","size image image",true,"result","string");
    cmd.add( sizeArg );

    ValueArg<std::string> outArg("o","output","output image", true,"","string");
    cmd.add( outArg );

    // Parse the args.
    cmd.parse( argc, argv );

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.SizeIm = sizeArg.getValue();

    }
  catch (ArgException &e)  // catch any exceptions
    {
    std::cerr << "error: " << e.error() << " for arg " << e.argId() << std::endl;
    }
}

#if 0
/////////////////////////////////
template <class MaskPixType, class SizePixType, int dim>
void doDilate(const CmdLineType &CmdLineObj)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;
  typedef typename itk::Image<SizePixType, dim> SizeImType;

  typedef typename itk::Image<float, dim> DistImType;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  typename SizeImType::Pointer dilsize = readIm<SizeImType>(CmdLineObj.SizeIm);

  // apply mask to size
  itk::Instance< itk::MaskImageFilter <SizeImType, MaskImType, SizeImType> > Masker;

  Masker->SetInput(dilsize);
  Masker->SetInput2(mask);

  // scale the size field to turn the range into a more reasonable
  // one. Normally we'd do this in the input image, but I created it
  // in matlab and it can't save any useful floating point formats

  itk::Instance< itk::DivideByConstantImageFilter <SizeImType, float, DistImType> > Scaler;
  Scaler->SetInput(Masker->GetOutput());
  Scaler->SetConstant(10);

  // square this to give distances in the right form
  itk::Instance< itk::SquareImageFilter <DistImType, DistImType> > Square;
  Square->SetInput(Scaler->GetOutput());

  writeIm<DistImType>(Square->GetOutput(), "square.mha");
  // dilate this with parabolic SE
  itk::Instance< itk::ParabolicDilateImageFilter<DistImType, DistImType> > Dilate;
  Dilate->SetInput(Square->GetOutput());
  Dilate->SetScale(0.5);

  // threshold at zero to get the mask
  itk::Instance< itk::BinaryThresholdImageFilter<DistImType, MaskImType> > Thresh;
  Thresh->SetInput(Dilate->GetOutput());
  Thresh->SetUpperThreshold(0);
  Thresh->SetLowerThreshold(0);
  Thresh->SetOutsideValue(1);
  Thresh->SetInsideValue(0);

  writeIm<MaskImType>(Thresh->GetOutput(), CmdLineObj.OutputIm);
  

}
#endif

template <class MaskPixType, class SizePixType, int dim>
void doDilate(const CmdLineType &CmdLineObj)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;
  typedef typename itk::Image<SizePixType, dim> SizeImType;

  typedef typename itk::Image<float, dim> DistImType;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  typename SizeImType::Pointer dilsize = readIm<SizeImType>(CmdLineObj.SizeIm);

  // apply mask to size
  itk::Instance< itk::MaskImageFilter <SizeImType, MaskImType, SizeImType> > Masker;

  Masker->SetInput(dilsize);
  Masker->SetInput2(mask);

  // scale the size field to turn the range into a more reasonable
  // one. Normally we'd do this in the input image, but I created it
  // in matlab and it can't save any useful floating point formats

  itk::Instance< itk::DivideByConstantImageFilter <SizeImType, float, DistImType> > Scaler;
  Scaler->SetInput(Masker->GetOutput());
  Scaler->SetConstant(10);

  // Variable dilation
  itk::Instance< itk::SpatVarDilateImageFilter<DistImType, MaskImType> > Dilate;
  Dilate->SetInput(Scaler->GetOutput());

  writeIm<MaskImType>(Dilate->GetOutput(), CmdLineObj.OutputIm + "_dil.tif");
  Dilate->writeDist("dist_var.mha");

}

template <class MaskPixType, class SizePixType, int dim>
void doErode(const CmdLineType &CmdLineObj)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;
  typedef typename itk::Image<SizePixType, dim> SizeImType;

  typedef typename itk::Image<float, dim> DistImType;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  typename SizeImType::Pointer dilsize = readIm<SizeImType>(CmdLineObj.SizeIm);

  // apply mask to size
  itk::Instance< itk::MaskNegatedImageFilter <SizeImType, MaskImType, SizeImType> > Masker;

  Masker->SetInput(dilsize);
  Masker->SetInput2(mask);

  // scale the size field to turn the range into a more reasonable
  // one. Normally we'd do this in the input image, but I created it
  // in matlab and it can't save any useful floating point formats

  itk::Instance< itk::DivideByConstantImageFilter <SizeImType, float, DistImType> > Scaler;
  Scaler->SetInput(Masker->GetOutput());
  Scaler->SetConstant(10);

  // Variable dilation
  itk::Instance< itk::SpatVarDilateImageFilter<DistImType, MaskImType> > Dilate;
  Dilate->SetInput(Scaler->GetOutput());

  itk::Instance< itk::MaskNegatedImageFilter <SizeImType, MaskImType, SizeImType> > Masker2;
  Masker2->SetInput(mask);
  Masker2->SetInput2(Dilate->GetOutput());

  writeIm<MaskImType>(Masker2->GetOutput(), CmdLineObj.OutputIm + "_ero.tif");
  
  //Dilate->writeDist("dist_var.mha");

}


/////////////////////////////////

int main(int argc, char * argv[])
{

  int dim1, dim2 = 0;
  CmdLineType CmdLineObj;
  ParseCmdLine(argc, argv, CmdLineObj);

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);


  itk::ImageIOBase::IOComponentType ComponentType, SizeComponentType;

  if (!readImageInfo(CmdLineObj.InputIm, &ComponentType, &dim1))
    {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return(EXIT_FAILURE);
    }
  if (!readImageInfo(CmdLineObj.SizeIm, &SizeComponentType, &dim2))
    {
    std::cerr << "Failed to open " << CmdLineObj.SizeIm << std::endl;
    return(EXIT_FAILURE);
    }

  if (dim1 != dim2)
    {
    std::cerr << "Image dimensions must match " << dim1 << " " << dim2 << std::endl;
    return(EXIT_FAILURE);

    }

  switch (dim1)
    {
    case 2:
      doDilate<unsigned char, unsigned char, 2>(CmdLineObj);
      doErode<unsigned char, unsigned char, 2>(CmdLineObj);
      break;
    case 3:
      doDilate<unsigned char, unsigned char, 3>(CmdLineObj);
      doErode<unsigned char, unsigned char, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
    }
  return EXIT_SUCCESS;
}
