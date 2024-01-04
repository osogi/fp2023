Copyright 2023-2024, Efremov Alexey 
SPDX-License-Identifier: CC0-1.0

  $ ./demoInterpret.exe < ./attachments/simple_ssa_fail.ll
  SSA check failed: 
  	Variable arg already was assigned

  $ ./demoInterpret.exe < ./attachments/fac.ll
  Programm return: 
  	 (CInteger (32, 120L))

  $ ./demoInterpret.exe 2000 20 4 < ./attachments/sum_args.ll
  Programm return: 
  	 (CInteger (32, 2024L))

  $ ./demoInterpret.exe 2000 -1000 30337 1 1 1 1 1 < ./attachments/sum_args.ll
  Programm return: 
  	 (CInteger (32, 31342L))

  $ ./demoInterpret.exe  12.0 0.0   0. 0.   0. 12.  < ./attachments/triangle_square.ll
  Programm return: 
  	 (CFloat 72.)

  $ ./demoInterpret.exe  0.012 0.    0. 0.   0. 0.012  < ./attachments/triangle_square.ll
  Programm return: 
  	 (CFloat 7.2e-05)

  $ ./demoInterpret.exe 1 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 1L))

  $ ./demoInterpret.exe 2 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 2L))

  $ ./demoInterpret.exe 6 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 720L))

  $ ./demoInterpret.exe 10 < ./attachments/fac_arg.ll 
  Programm return: 
  	 (CInteger (32, 3628800L))
