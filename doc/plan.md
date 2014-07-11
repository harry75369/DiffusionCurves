# Research Plan

Vector graphics is important. With the development of screen of higher and 
higher resolution, the shortcomings of raster graphics are becomming more and 
more obvious, e.g. limited resolution, big file size and the most important, 
the mosaic effect. Although mosaic effect can be seen as a kind of art 
sometimes, it is not wanted in most use cases. In contrast, vector graphics has 
unlimited resolution and smaller file size, and it also can eliminate mosaic 
effect at arbitrary resolution.

One shortcoming for vector graphics, however, is that it takes more overheads 
to get rendered compared with raster graphics[?]. This problem is further 
worsen by the apperance of diffusion curve, which is a new primitive in
vector graphics for arbitrary gradual color change effect but requires more 
computing power. Fortunately, with modern hardware and advanced algorithms, 
this problem is no longer a crucial one. The crucial problems for now are as 
follows:

 * How can vector graphics achieve the same quality of raster graphics? Here 
   the quality means the expressive richness provided by raster graphics.

 * How can vector graphics transcend the expressive level of raster graphics, 
   maybe through non-photorealistic redering techniques and techiques that well
   utilizes the human vision system?

 * Is there any possibility for new vector graphics primitives, for example, 
   "advection" curve inspired by "diffusion" curve?





# Project Plan

## Fundamental

 * Implement Diffusion Curves with FMM in C++. [2014/07-2014/08]

## Application

 * Web app for artist creation with Diffusion Curves.
 * Animation

## Extension

 * New primitives for Vector Graphics.
