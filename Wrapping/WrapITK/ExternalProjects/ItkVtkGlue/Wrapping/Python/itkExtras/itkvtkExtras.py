
# display scalar images in volume
class show3D :
  def __init__(self, input=None, MinOpacity=0.0, MaxOpacity=0.1) :
    import qt
    import vtk
    from vtk.qt.QVTKRenderWindowInteractor import QVTKRenderWindowInteractor
    self.__MinOpacity__ = MinOpacity
    self.__MaxOpacity__ = MaxOpacity
    # every QT app needs an app
    self.__app__ = qt.QApplication(['itkviewer'])
    # create the widget
    self.__widget__ = QVTKRenderWindowInteractor()
    self.__ren__ = vtk.vtkRenderer()
    self.__widget__.GetRenderWindow().AddRenderer(self.__ren__)
    self.__itkvtkConverter__ = None
    self.__volumeMapper__ = vtk.vtkVolumeTextureMapper2D()
    self.__volume__ = vtk.vtkVolume()
    self.__volumeProperty__ = vtk.vtkVolumeProperty()
    self.__volume__.SetMapper(self.__volumeMapper__)
    self.__volume__.SetProperty(self.__volumeProperty__)
    self.__ren__.AddVolume(self.__volume__)
    self.__outline__ = None
    self.__outlineMapper__ = None
    self.__outlineActor__ = None
    self.AdaptColorAndOpacity(0, 255)
    if input :
      self.SetInput(input)
      self.AdaptColorAndOpacity()
      
  def Render(self):
    self.__ren__.Render()
    
  def GetWidget(self) :
    return self.__widget__
  
  def GetRenderer(self) :
    return self.__ren__
  
  def GetConverter(self) :
    return self.__itkvtkConverter__
  
  def GetVolumeMapper(self) :
    return self.__volumeMapper__
  
  def GetVolume(self) :
    return self.__volume__
  
  def GetVolumeProperty(self) :
    return self.__volumeProperty__
  
  def Show(self) :
    self.__widget__.show()
    
  def Hide(self) :
    self.__widget__.hide()
    
  def SetInput(self, input) :
    import itk
    img = itk.image(input)
    self.__input__ = img
    if img :
      # Update to try to avoid to exit if a c++ exception is throwed
      # sadely, it will not prevent the program to exit later...
      # a real fix would be to wrap c++ exception in vtk
      img.UpdateOutputInformation()
      img.Update()
      
      # flip the image to get the same representation than the vtk one
      self.__flipper__ = itk.FlipImageFilter[img].New(Input=img)
      axes = self.__flipper__.GetFlipAxes()
      axes.SetElement(1, True)
      self.__flipper__.SetFlipAxes(axes)
      
      # change the spacing while still keeping the ratio to workaround vtk bug
      # when spacing is very small
      spacing_ = itk.spacing(img)
      normSpacing = []
      for i in range(0, spacing_.Size()):
        normSpacing.append( spacing_.GetElement(i) / spacing_.GetElement(0) )
      self.__changeInfo__ = itk.ChangeInformationImageFilter[img].New(self.__flipper__, OutputSpacing=normSpacing, ChangeSpacing=True)
      
      # now really convert the data
      self.__itkvtkConverter__ = itk.ImageToVTKImageFilter[img].New(self.__changeInfo__)
      self.__volumeMapper__.SetInput(self.__itkvtkConverter__.GetOutput())
      # needed to avoid warnings
      # self.__itkvtkConverter__.GetOutput() must be callable
      
      import vtk
      if not self.__outline__ :
	  self.__outline__ = vtk.vtkOutlineFilter()
	  self.__outline__.SetInput(self.__itkvtkConverter__.GetOutput())
	  self.__outlineMapper__ = vtk.vtkPolyDataMapper()
	  self.__outlineMapper__.SetInput(self.__outline__.GetOutput())
	  self.__outlineActor__ = vtk.vtkActor()
	  self.__outlineActor__.SetMapper(self.__outlineMapper__)
	  self.__ren__.AddActor(self.__outlineActor__)
      else :
	  self.__outline__.SetInput(self.__itkvtkConverter__.GetOutput())

    self.Render()
    
  def GetInput(self):
    return self.__input__
  
  def AdaptColorAndOpacity(self, minVal=None, maxVal=None):
    if minVal == None or maxVal == None :
      m, M = self.GetRange()
      if minVal == None :
        minVal = m
      if maxVal == None :
        maxVal = M
    self.AdaptOpacity(minVal, maxVal)
    self.AdaptColor(minVal, maxVal)
    
  def AdaptOpacity(self, minVal=None, maxVal=None) :
    import vtk
    if minVal == None or maxVal == None :
      m, M = self.GetRange()
      if minVal == None :
        minVal = m
      if maxVal == None :
        maxVal = M
    opacityTransferFunction = vtk.vtkPiecewiseFunction()
    opacityTransferFunction.AddPoint(minVal, self.__MinOpacity__)
    opacityTransferFunction.AddPoint(maxVal, self.__MaxOpacity__)
    self.__volumeProperty__.SetScalarOpacity(opacityTransferFunction)
    
  def AdaptColor(self, minVal=None, maxVal=None):
    import vtk
    if minVal == None or maxVal == None :
      m, M = self.GetRange()
      if minVal == None :
        minVal = m
      if maxVal == None :
        maxVal = M
    colorTransferFunction = vtk.vtkColorTransferFunction()
    colorTransferFunction.AddHSVPoint(minVal, 0.0, 0.0, 0.0)
    colorTransferFunction.AddHSVPoint((maxVal-minVal)*0.25, 0.66, 1.0, 1.0)
    colorTransferFunction.AddHSVPoint((maxVal-minVal)*0.5,  0.44, 1.0, 1.0)
    colorTransferFunction.AddHSVPoint((maxVal-minVal)*0.75, 0.22, 1.0, 1.0)
    colorTransferFunction.AddHSVPoint(maxVal,               0.0,  1.0, 1.0)
    self.__volumeProperty__.SetColor(colorTransferFunction)
    self.Render()
    
  def GetRange(self) :
    conv = self.GetConverter()
    conv.Update()
    return conv.GetOutput().GetScalarRange()
  
  def GetMaxOpacity(self) :
    return self.__MaxOpacity__
  
  def GetMinOpacity(self) :
    return self.__MinOpacity__
  
  def SetMaxOpacity(self, val) :
    self.__MaxOpacity__ = val
    self.AdaptColorAndOpacity()
    
  def SetMinOpacity(self, val) :
    self.__MinOpacity__ = val
    self.AdaptColorAndOpacity()



import itkExtras

class lsm( itkExtras.pipeline ):
  """ Use vtk to import LSM image in ITK.
  """
  def __init__(self, fileName=None, channel=0, ImageType=None ):
    from vtk import vtkLSMReader, vtkImageCast
    import itk
    itk.pipeline.__init__(self)
    # if ImageType is None, give it a default value
    # this is useful to avoid loading Base while loading this module
    if ImageType == None:
      ImageType = itk.Image.UC3
    # remove useless SetInput() method created by the constructor of the pipeline class
#     del self.SetInput
    # set up the pipeline
    self.connect( vtkLSMReader() )
    self.connect( vtkImageCast() )
    PType = itk.template(ImageType)[1][0]
    if PType == itk.UC:
      self[-1].SetOutputScalarTypeToUnsignedChar()
    elif PType == itk.US:
      self[-1].SetOutputScalarTypeToUnsignedShort()
    self.connect( itk.VTKImageToImageFilter[ImageType].New() )
    self.connect( itk.ChangeInformationImageFilter[ImageType].New( ChangeSpacing=True ) )
    # and configure the pipeline
    if fileName:
      self.SetFileName( fileName )
    self.SetChannel( channel )

  def SetFileName( self, fileName ):
    self[0].SetFileName( fileName )
    self[0].Update()
    self.UpdateSpacing()

  def SetChannel( self, channel ):
    self[0].SetUpdateChannel( channel )
    self[0].Update()
    self.UpdateSpacing()
    self.__channel__ = channel
    return self.GetChannelName( channel )

  def UpdateSpacing(self):
    spacing = self[0].GetVoxelSizes()
    spacing = [ v * 1e6 for v in spacing ]
    self[-1].SetOutputSpacing( spacing )

  def GetFileName(self):
    return self[0].GetFileName()
  
  def GetChannel(self):
    return self.__channel__
  
  def GetNumberOfChannels(self):
    return self[0].GetNumberOfChannels()
  
  def GetChannelName(self, channel=None):
    if channel == None:
      channel = self.GetChannel()
    return self[0].GetChannelName( channel )
  
del itkExtras
