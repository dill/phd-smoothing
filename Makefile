#########################################################

# REPLACE the main_prg_name below with the name of
# your main program without the ".f90" extension
# REPLACE the module_name below with your module 
# name without the ".f90" extension

# PROGRAM NAME (no .f90)
main=polysc

# MODULE NAME  (no .f90)
mod1=scpack
mod2=sclib

#########################################################

cmplr=gfortran -ffixed-form -g

objects1 = $(mod1).o $(mod2).o $(main).o

$(main)   : $(objects1)
	$(cmplr) -o $(main) $(objects1)

$(mod1).o    : $(mod1).f90
	$(cmplr) -c $(mod1).f90

$(mod2).o    : $(mod2).f90
	$(cmplr) -c $(mod2).f90

$(main).o  : $(main).f90 $(mod1).f90 $(mod2).f90
	$(cmplr) -c $(main).f90


clean :
	rm -f *.mod *.pcl *.pc *.o $(main) *.inc *.vo *.d 
