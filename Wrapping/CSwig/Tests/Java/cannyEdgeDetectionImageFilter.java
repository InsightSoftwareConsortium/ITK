import InsightToolkit.*;

// This example illustrates how C++ classes can be used from Java using SWIG.
// The Java class gets mapped onto the C++ class and behaves as if it is a Java class.

public class cannyEdgeDetectionImageFilter {
  public static void main(String argv[])
  {
    itkImageFileReaderF2_Pointer reader = itkImageFileReaderF2.itkImageFileReaderF2_New();
    itkCannyEdgeDetectionImageFilterF2F2_Pointer canny
      = itkCannyEdgeDetectionImageFilterF2F2.itkCannyEdgeDetectionImageFilterF2F2_New();
    itkRescaleIntensityImageFilterF2US2_Pointer rescaler
      = itkRescaleIntensityImageFilterF2US2.itkRescaleIntensityImageFilterF2US2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();
    canny.SetInput(reader.GetOutput());
    rescaler.SetInput(canny.GetOutput());
    writer.SetInput(rescaler.GetOutput());
    rescaler.SetOutputMinimum(0);
    rescaler.SetOutputMaximum(65535);
    reader.SetFileName("../../../../Testing/Data/Input/cthead1.png");
    writer.SetFileName("./testout.png");
    writer.Update();
  }
}
