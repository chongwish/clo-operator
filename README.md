# NAME

clo-operator - A Additional Operator For Common Lisp

# SYNOPSIS

    clo => Common Lisp Only
    operator => Make the behavior of operator like C/C++ language or map to other fn/macro

# Usage

    # load to system
    # link to the asd or ql path
    $> ln -s {clo-operator-path} {asd/ql-path}
    # or copy
    $> copy {clo-operator-path} {asd/ql-path}
  
    # bit: auto generate 8-bit and 32-bit
    # generate operator (like C/C++) templates
    # generate 4-bit operator => int4
    CL-User> (clo-operator.bit:template 4)
    CL-User> (clo-operator.bit:[4]+ 5 8)
    13
    CL-User> (clo-operator.bit:[8]+ 1000000 6000000)
    192
  
    # endian: little endian <=> big endian
    CL-User> (clo-operator.endian:transform 16)
    268435456

    # fn: generate fn map helper
    # macro: generate macro map helper
    # pkg: find and set fn/macro in a package