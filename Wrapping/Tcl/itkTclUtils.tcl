package require WrapTclFacility 1.0

proc ItkNew {type} {
  itkSmartPointer_$type __tptr [$type New]
  set t [__tptr GetPointer]
  $t Register
  wrap::Delete __tptr
  return $t
}

proc ItkDelete {ptr} {
  $ptr UnRegister
  rename $ptr {}
}

proc ItkListCommands {} {
  foreach i [lsort [info commands itk*]] { puts "$i" }
}

