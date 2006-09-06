/**
 *  Example on the use of the BinaryErodeImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryErodeImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryErodeImageFilter Example");

    itkImageFileReaderIUS2_Pointer reader = itkImageFileReaderIUS2.itkImageFileReaderIUS2_New();
    itkImageFileWriterIUC2_Pointer writer = itkImageFileWriterIUC2.itkImageFileWriterIUC2_New();
    itkCastImageFilterIUS2IUC2_Pointer caster = itkCastImageFilterIUS2IUC2.itkCastImageFilterIUS2IUC2_New();

    itkBinaryErodeImageFilterIUS2IUS2SE2_Pointer filter = itkBinaryErodeImageFilterIUS2IUS2SE2.itkBinaryErodeImageFilterIUS2IUS2SE2_New();

    filter.SetInput( reader.GetOutput() );
    caster.SetInput( filter.GetOutput() );
    writer.SetInput( caster.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    itkSize2 radius = new itkSize2();
    radius.Fill( 5 );
    itkFlatStructuringElement2 element = itkFlatStructuringElement2.Ball( radius );

    filter.SetKernel( element );

    short value = 200;

    filter.SetErodeValue( value );

    writer.Update();
  }

}


