#Poisson example
#Introduction
This example approximately solves an example of Poisson's equation using a simple version of the Gauss-Seidel method. It mainly uses the terminology of the electrostatic form of the equation but has normalised out the permitivity. You can specify the number of grid points in X and Y (it is recommended that these are kept small!) and the function form for the charge density rho. This example uses root keys to allow the deck to be simple key-value pairs without any blocks.

##Extensions
* Add code to allow you to set the boundary conditions from the deck. This should be very similar to the code for setting the charge density but apply along each edge deliberately. Note that this example has the domain running (1->nx, 1->ny) and allocates one strip of guard cells outside that.
* Switch from using a global variable for getting the x and y positions to using host parameters. This will involve writing a getter function but will make the code re-entrant so that it can be more easily used in an threaded environment
