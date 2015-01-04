The face3d Package is based on "rgl" package, and implement 3d human face visualization and animation. People can use this package to plot 3d human faces with different gradient color, different transformation of the face and a amazing animation to show the face transformation.


For common plot, just call read.obj() to read a .obj file to construct an obj object, and call create.obj3d() to create a 3d face object for plot.
Moreover, function colMesh3d() provides function of making gradient colours for ploting 3d object created by create.obj3d()

For animation plot, we use partial least square to implement the dynamic transformation by add/subtract the standard deviation of the faces sample, function get.loading needs loading file, standard deviation file. Since this function is a specific function only used in our study, which subject to relationship between face and human personalities, so here we just provide the example file for plot animation, and you can modify the code for your personal purposes.
