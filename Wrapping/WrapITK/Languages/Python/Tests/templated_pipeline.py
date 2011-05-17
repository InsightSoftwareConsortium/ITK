import itk
from sys import argv
itk.auto_progress(2)

# define a custom templated pipeline

class LabelDilateImageFilter(itk.pipeline):
  def __init__(self, *args, **kargs):
    # call the constructor of the superclass but without args and kargs, because the attributes
    # are not all already there!
    # Set/GetRadius() is created in the constructor for example, with the expose() method
    itk.pipeline.__init__(self)

    # get the template parameters
    template_parameters = kargs["template_parameters"]
    # check the template parameters validity. Not really useful in that case, because we do the same
    # here, but a good habit
    LabelDilateImageFilter.check_template_parameters(template_parameters)

    # and store them in an easier way
    ImageType, DistanceMapType = template_parameters

    # build the minipipeline
    self.connect(itk.DanielssonDistanceMapImageFilter[ImageType, DistanceMapType].New(UseImageSpacing=True, SquaredDistance=False))
    self.connect(itk.BinaryThresholdImageFilter[DistanceMapType, ImageType].New())
    self.expose("UpperThreshold", "Radius")
    self.append(itk.CastImageFilter[DistanceMapType, ImageType].New(self.filters[0].GetVoronoiMap()))
    self.connect(itk.MaskImageFilter[ImageType, ImageType, ImageType].New(Input2=self.filters[1]))

    # now we can parse the inputs
    itk.set_inputs(self, args, kargs)

  def check_template_parameters(template_parameters):
    ImageType, DistanceMapType = template_parameters
    itk.DanielssonDistanceMapImageFilter[ImageType, DistanceMapType]
    itk.BinaryThresholdImageFilter[DistanceMapType, ImageType]
    itk.CastImageFilter[DistanceMapType, ImageType]
    itk.MaskImageFilter[ImageType, ImageType, ImageType]
  check_template_parameters = staticmethod(check_template_parameters)

LabelDilateImageFilter = itk.templated_class(LabelDilateImageFilter)


# and use it
dim = 2
IType = itk.Image[itk.UC, dim]
OIType = itk.Image[itk.UC, dim]
DIType = itk.Image[itk.F, dim]

reader = itk.ImageFileReader[IType].New( FileName=argv[1] )
dilate = LabelDilateImageFilter[IType, DIType].New(reader, Radius=eval(argv[3]))
writer = itk.ImageFileWriter[OIType].New( dilate, FileName=argv[2] )

writer.Update()
