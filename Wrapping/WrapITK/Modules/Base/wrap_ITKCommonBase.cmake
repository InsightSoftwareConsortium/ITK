WRAP_NON_TEMPLATE_CLASS("itk::LightObject"        POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::Object"             POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::DataObject"         POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::LightProcessObject" POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::ProcessObject"      POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::Command"            POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::Directory"          POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::DynamicLoader"      POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::ObjectFactoryBase"  POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::OutputWindow"       POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::Version"            POINTER)
WRAP_NON_TEMPLATE_CLASS("itk::TimeStamp")
WRAP_NON_TEMPLATE_CLASS("itk::Indent")
WRAP_NON_TEMPLATE_CLASS("itk::SimpleFilterWatcher")
WRAP_NON_TEMPLATE_CLASS("itk::XMLFilterWatcher")
WRAP_NON_TEMPLATE_CLASS("itk::StringStream")

#
# disabled to fix too long name on some systems with java
# see http://public.kitware.com/pipermail/insight-users/2006-July/018785.html
#
# WRAP_NON_TEMPLATE_CLASS("itk::MetaDataDictionary")

