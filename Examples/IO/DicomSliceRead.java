/**
 * Example on the use of DicomImageIO for reading a single DICOM slice, rescale
 * the intensities and save it in a different file format.
 *
 */

import InsightToolkit.*;

public class DicomSliceRead
{
  public static void main( String argv[] )
  {
    System.out.println("DicomSliceRead Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUC2_Pointer writer = itkImageFileWriterUC2.itkImageFileWriterUC2_New();

    itkRescaleIntensityImageFilterUS2UC2_Pointer filter = itkRescaleIntensityImageFilterUS2UC2.itkRescaleIntensityImageFilterUS2UC2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    itkDicomImageIO_Pointer dicomIO = itkDicomImageIO.itkDicomImageIO_New();
    
    reader.SetImageIO( dicomIO.GetPointer() );
    
    filter.SetOutputMinimum( (short)0 );
    filter.SetOutputMaximum( (short) 255);

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


