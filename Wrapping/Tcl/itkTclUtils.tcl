package require WrapTclFacility 0.01

proc ItkNew {type} {
  set tptr [itkSmartPointer_$type [$type New]]
  set t [$tptr GetPointer]
  $t Register
  set tptr {}
  return $t
}

proc ItkDelete {ptr} {
  $ptr UnRegister
}

proc ItkListCommands {} {
  foreach i [lsort [info commands itk*]] { puts "$i" }
}
